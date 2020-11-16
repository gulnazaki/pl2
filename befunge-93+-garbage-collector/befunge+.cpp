#include <stack>
#include <vector>
#include <stdio.h>
#include <stdlib.h>
#include <time.h> 

#define W 80
#define H 25
#define MAX_STACK (1 << 20)
#define MAX_HEAP (1 << 24)
#define RANDOM (rand() >> 5) & 3
#define PROGRESS x = (x + dx + W) % W; y = (y + dy + H) % H; pc = program[x][y]; label = label_tab[pc]
#define NEXT_INSTRUCTION goto *(void *)(label)

typedef signed long long sll;

#define AS_PTR(val) (((val) << 2) | 1)
#define AS_NUM(val) ((val) << 2)
#define IS_PTR(val) ((val) & 1)
#define TO_NUM(val) ((val) >> 2)
#define IS_MARKED(val) (heap[(val)].head & 2)
#define MARK(val) (heap[(val)].head |= 2)
#define UNMARK(val) (heap[(val)].head &= (~2))

#define PUSH(val) \
	if (stack.size() < max_stack) { \
		stack.push(val); \
	} \
	else { \
		printf("\nStack overflow\n"); \
		exit(5); \
	}

#define POP \
	(stack.empty() ? 0 : (value = stack.top(), stack.pop(), TO_NUM(value)))

#define POP_RAW \
	(stack.empty() ? 0 : (value = stack.top(), stack.pop(), value))

#define GET_G \
	if (a >= W || b >= H || x < 0 || y < 0) { \
		printf("%lld:%lld Command g tried to get value from an out of limits area\n", b, a); \
		exit(3); \
	} \
	PUSH(AS_NUM(program[a][b]))

#define PUT_P \
	if (a >= W || b >= H || a < 0 || b < 0) { \
		printf("%lld:%lld Command p tried to put a value in an out of limits area\n", b, a); \
		exit(4); \
	} \
	program[a][b] = POP;

static int max_stack = MAX_STACK;
static int max_heap = MAX_HEAP;

struct Cell { 
    sll head;
    sll tail; 
};

class memory_manager {
private:
	std::vector<Cell> heap;
	sll freelist_start = -1;
	sll freelist_end;
	std::stack<sll> dfs_stack;

	// dfs with explicit stack
	void mark(sll index) {
		sll a, b;

		MARK(index);
		dfs_stack.push(index);
		while(!dfs_stack.empty()) {
			index = dfs_stack.top();
			dfs_stack.pop();
			a = heap[index].head;
			b = heap[index].tail;
			if (IS_PTR(a)) {
				index = TO_NUM(a);
				if (!IS_MARKED(index)) {
					MARK(index);
					dfs_stack.push(index);
				}
			}
			if (IS_PTR(b)) {
				index = TO_NUM(b);
				if (!IS_MARKED(index)) {
					MARK(index);
					dfs_stack.push(index);
				}
			}
		}
	}

	// uses a DIY memory-cost-friendly "linked-list" (storing starting and ending indices)
	sll sweep() {
		sll heap_size = heap.size();
		sll i, sweeped = 0;
		for (i = 0; i < heap_size; i++) {
			if (IS_MARKED(i)) UNMARK(i);
			else {
				if (freelist_start < 0) {
					freelist_start = freelist_end = i;
				}
				else {
					heap[freelist_end].head = i;
					freelist_end = i;
				}
			sweeped++;
			}
		}
		return sweeped;
	}

	void collect(std::stack<sll> stack) {
		#ifdef DEBUG
		printf("\nGarbage Collector Started\n"); fflush(stdout);
		#endif
		sll address, sweeped;

		while (!stack.empty()) {
			if (IS_PTR(stack.top())) {
				address = TO_NUM(stack.top());
				if (!IS_MARKED(address)) {
					mark(address);
				}
			}
			stack.pop();
		}

		sweeped = sweep();
		#ifdef DEBUG
		printf("\nDone! Sweeped %lld/%ld\n",sweeped, heap.size()); fflush(stdout);
		#endif
	}

	sll use_the_freelist(Cell cell) {
		sll address = freelist_start;
		sll next_free = heap[freelist_start].head;
		heap[freelist_start] = cell;
		if (freelist_start != freelist_end)	freelist_start = next_free;
		else 								freelist_start = -1;
		return address;
	}

public:
	sll allocate(sll head, sll tail, std::stack<sll> stack) {
		if (heap.size() < max_heap) {
			Cell cell = {head, tail};
			heap.push_back(cell);
			return heap.size() - 1;
		}
		else {
			if (freelist_start >= 0) {
				return use_the_freelist({head, tail});
			}
			else {
				// at this point allocation failed (heap is full and freelist is empty)
				if (IS_PTR(head)) stack.push(head);
				if (IS_PTR(tail)) stack.push(tail);
				collect(stack);
				// if garbage collection can't save us we are out of memory for real
				if (freelist_start < 0) {
					printf("\nOut of memory\n");
					exit(6);
				}
				else {
					return use_the_freelist({head, tail});
				}
			}
		}
	}

	sll get_head(sll address) {
		sll index = TO_NUM(address);
		if (!IS_PTR(address) || index >= heap.size()) {
			printf("%lld is not a valid address\n", address);
			exit(7);
		}
		return heap[index].head;
	}

	sll get_tail(sll address) {
		sll index = TO_NUM(address);
		if (!IS_PTR(address) || index >= heap.size()) {
			printf("%lld is not a valid address\n", address);
			exit(7);
		}
		return heap[index].tail;
	}
};

memory_manager mem;

int main(int argc, char **argv) {
	if (argc != 2) {
		printf("This Befunge-93 interpreter expects one program as argument, you supplied %d arguments\n", argc -1);
		exit(1);
	}

	FILE *fp;
	fp = fopen(argv[1], "r");
	
	if (!fp) {
		printf("Couldn't read your program\n");
		exit(1);
	}

	static void *label_tab[128];
	static void *label;
	std::fill(label_tab, label_tab + 128, &&invalid);	
	std::fill(label_tab + '0', label_tab + ':', &&num);
	label_tab[' '] = &&nop;
	label_tab['+'] = &&add;
	label_tab['-'] = &&sub;
	label_tab['*'] = &&mult;
	label_tab['/'] = &&div;
	label_tab['%'] = &&mod;
	label_tab['!'] = &&not_;
	label_tab['`'] = &&comp;
	label_tab['>'] = &&right;
	label_tab['<'] = &&left;
	label_tab['v'] = &&down;
	label_tab['^'] = &&up;
	label_tab['?'] = &&rand;
	label_tab['_'] = &&iflr;
	label_tab['|'] = &&ifud;
	label_tab['"'] = &&string;
	label_tab[':'] = &&dup;
	label_tab['\\'] = &&swap;
	label_tab['$'] = &&pop_;
	label_tab['.'] = &&printnum;
	label_tab[','] = &&printchar;
	label_tab['#'] = &&skip;
	label_tab['g'] = &&get;
	label_tab['p'] = &&put;
	label_tab['&'] = &&scannum;
	label_tab['~'] = &&scanchar;
	label_tab['c'] = &&cons;
	label_tab['h'] = &&head;
	label_tab['t'] = &&tail;
	label_tab['@'] = &&theend;

	char program[W][H];
	std::stack<sll> stack;
	int x, y, dx, dy;
	sll a, b, c, value;
	char ch, pc;
	
	for (y=0; y<H; y++) {
		for (x=0; x<W; x++) {
			program[x][y] = ' ';
		};
	}

	x = y = 0;
	while(1) {
		ch = fgetc(fp);
		if (ch == EOF) break;
		if (ch == '\n') {
			y++;
			x = 0;
		}
		else {
			if (y >= H) {
				printf("Too many lines, Befunge-93 programs shouldn't exceed %dx%d characters\n", W, H);
				exit(2);
			}
			if (x >= W) {
				printf("Line %d too long, Befunge-93 programs shouldn't exceed %dx%d characters\n", y, W, H);
				exit(2);
			}
			program[x][y] = ch;
			x++;
		}
	}
	fclose(fp);

	srand(time(0));

	x = y = dy = 0;
	dx = 1;
	pc = program[x][y];
	while (1) {
		switch (pc) {
			case ' ' : 
			nop:
				PROGRESS;
				NEXT_INSTRUCTION;
				break;
			case '0' : case '1' : case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
			num:
				ch = pc;
				PROGRESS;
				PUSH(AS_NUM(ch - '0'));
				NEXT_INSTRUCTION;
				break;
			case '+' :
			add:
				PROGRESS;
				PUSH(AS_NUM(POP + POP));
				NEXT_INSTRUCTION;
				break;
			case '-' :
			sub:
				PROGRESS;
				a = POP;
				PUSH(AS_NUM(POP - a));
				NEXT_INSTRUCTION;
				break;
			case '*' :
			mult:	
				PROGRESS;
				PUSH(AS_NUM(POP * POP));
				NEXT_INSTRUCTION;
				break;
			case '/' :
			div:
				PROGRESS;
				a = POP;
				PUSH(AS_NUM(POP / a));
				NEXT_INSTRUCTION;
				break;
			case '%' :
			mod:
				PROGRESS;
				a = POP;
				PUSH(AS_NUM(POP % a));
				NEXT_INSTRUCTION;
				break;
			case '!' :
			not_:
				PROGRESS;
				PUSH(AS_NUM(POP ? 0 : 1));
				NEXT_INSTRUCTION;
				break;
			case '`' :
			comp:
				PROGRESS;
				a = POP;
				PUSH(AS_NUM(POP > a));
				NEXT_INSTRUCTION;
				break;
			case '>' :
			right:
				dx = 1;
				dy = 0;
				PROGRESS;
				NEXT_INSTRUCTION;
				break;
			case '<' :
			left:
				dx = -1;
				dy = 0;
				PROGRESS;
				NEXT_INSTRUCTION;
				break;
			case 'v' :
			down:
				dx = 0;
				dy = 1;
				PROGRESS;
				NEXT_INSTRUCTION;
				break;
			case '^' :
			up:
				dx = 0;
				dy = -1;
				PROGRESS;
				NEXT_INSTRUCTION;
				break;
			case '?' :
			rand:
				switch (RANDOM) {
					case 0 : dx = 1; dy = 0; break;
					case 1 : dx = -1; dy = 0; break;
					case 2 : dx = 0; dy = 1; break;
					case 3 : dx = 0; dy = -1; break;
				}
				PROGRESS;
				NEXT_INSTRUCTION;
				break;
			case '_' :
			iflr:
				dx = POP ? -1 : 1;
				dy = 0;
				PROGRESS;
				NEXT_INSTRUCTION;
				break;
			case '|' :
			ifud:
				dy = POP ? -1 : 1;
				dx = 0;					
				PROGRESS;
				NEXT_INSTRUCTION;
				break;
			case '"' :
			string:
				do {
					PROGRESS;
					PUSH(AS_NUM(pc));
				}
				while (pc != '"');
				stack.pop();
				PROGRESS;
				NEXT_INSTRUCTION;
				break;
			case ':' :
			dup:
				PROGRESS;
				a = POP_RAW;
				PUSH(a);
				PUSH(a);
				NEXT_INSTRUCTION;
				break;
			case '\\' :
			swap:
				PROGRESS;
				a = POP_RAW;
				b = POP_RAW;
				PUSH(a);
				PUSH(b);
				NEXT_INSTRUCTION;
				break;
			case '$' :
			pop_:
				PROGRESS;
				if (!stack.empty()) stack.pop();
				NEXT_INSTRUCTION;
				break;
			case '.' :
			printnum:
				PROGRESS;
				printf("%lld", POP);
				fflush(stdout);
				NEXT_INSTRUCTION;
				break;
			case ',' :
			printchar:
				PROGRESS;
				printf("%c", (char)POP);
				fflush(stdout);
				NEXT_INSTRUCTION;
				break;
			case '#' :
			skip:
				x += dx;
				y+= dy;
				PROGRESS;
				NEXT_INSTRUCTION;
				break;
			case 'g' :
			get:
				PROGRESS;
				b = POP;
				a = POP;
				GET_G;
				NEXT_INSTRUCTION;
				break;
			case 'p' :
			put:
				PROGRESS;
				b = POP;
				a = POP;
				PUT_P;
				NEXT_INSTRUCTION;
				break;
			case '&':
			scannum:
				PROGRESS;
				ch = scanf("%lld", &c);
				PUSH(AS_NUM(c));
				NEXT_INSTRUCTION;
				break;
			case '~':
			scanchar:
				PROGRESS;
				PUSH(AS_NUM(getchar()));
				NEXT_INSTRUCTION;
				break;
			case 'c':
			cons:
				PROGRESS;
				b = POP_RAW;
				a = POP_RAW;
				PUSH(AS_PTR(mem.allocate(a, b, stack)));
				NEXT_INSTRUCTION;
				break;
			case 'h':
			head:
				PROGRESS;
				PUSH(mem.get_head(POP_RAW));
				NEXT_INSTRUCTION;
				break;
			case 't':
			tail:
				PROGRESS;
				PUSH(mem.get_tail(POP_RAW));
				NEXT_INSTRUCTION;
				break;
			case '@':
			theend:
				return 0;
			default:
			invalid:
				printf("Invalid Befunge-93 Command!!!\n");
				exit(666);
		}
	}
}
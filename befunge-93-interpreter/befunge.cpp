#include <stack>
#include <stdio.h>
#include <stdlib.h>
#include <time.h> 

#define W 80
#define H 25
#define RANDOM (rand() >> 5) & 3
#define PROGRESS x = (x + dx + W) % W; y = (y + dy + H) % H; pc = program[x][y]; label = label_tab[pc]
#define NEXT_INSTRUCTION goto *(void *)(label)

#define POP \
	(stack.empty() ? 0 : (value = stack.top(), stack.pop(), value))

#define GET_G \
	if (a >= W || b >= H || x < 0 || y < 0) { \
		printf("%ld:%ld Command g tried to get value from an out of limits area\n", b, a); \
		exit(3); \
	} \
	stack.push(program[a][b])

#define PUT_P \
	if (a >= W || b >= H || a < 0 || b < 0) { \
		printf("%ld:%ld Command p tried to put a value in an out of limits area\n", b, a); \
		exit(4); \
	} \
	program[a][b] = POP;


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
	label_tab['@'] = &&theend;

	char program[W][H];
	std::stack<signed long> stack;
	int x, y, dx, dy;
	signed long a, b, c, value;
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
				stack.push(ch - '0');
				NEXT_INSTRUCTION;
				break;
			case '+' :
			add:
				PROGRESS;
				stack.push(POP + POP);
				NEXT_INSTRUCTION;
				break;
			case '-' :
			sub:
				PROGRESS;
				a = POP;
				stack.push(POP - a);
				NEXT_INSTRUCTION;
				break;
			case '*' :
			mult:	
				PROGRESS;
				stack.push(POP * POP);
				NEXT_INSTRUCTION;
				break;
			case '/' :
			div:
				PROGRESS;
				a = POP;
				stack.push(POP / a);
				NEXT_INSTRUCTION;
				break;
			case '%' :
			mod:
				PROGRESS;
				a = POP;
				stack.push(POP % a);
				NEXT_INSTRUCTION;
				break;
			case '!' :
			not_:
				PROGRESS;
				stack.push(POP ? 0 : 1);
				NEXT_INSTRUCTION;
				break;
			case '`' :
			comp:
				PROGRESS;
				a = POP;
				stack.push(POP > a);
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
					stack.push(pc);
				}
				while (pc != '"');
				stack.pop();
				PROGRESS;
				NEXT_INSTRUCTION;
				break;
			case ':' :
			dup:
				PROGRESS;
				a = POP;
				stack.push(a);
				stack.push(a);
				NEXT_INSTRUCTION;
				break;
			case '\\' :
			swap:
				PROGRESS;
				a = POP;
				b = POP;
				stack.push(a);
				stack.push(b);
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
				printf("%ld", POP);
				NEXT_INSTRUCTION;
				break;
			case ',' :
			printchar:
				PROGRESS;
				printf("%c", (char)POP);
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
				ch = scanf("%ld", &c); stack.push(c);
				NEXT_INSTRUCTION;
				break;
			case '~':
			scanchar:
				PROGRESS;
				stack.push(getchar());
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
/* Version of Frama-C: Argon 18.0
 * 
 * Runs with:
 * frama-c -wp -wp-prover alt-ergo -wp-rte -wp-timeout 300 -wp-verbose 0 samenum.cpp -then -report
 */

#define MAXN 1000000
#define MAXV 2000000

/*@ predicate distance{L}(integer N, int *x, integer d) = 
	\exists integer i; 0 <= i < N &&
    	\exists integer j; 0 <= j < i &&
    		x[i] == x[j] &&
    		d == i - j;
*/

/*@ predicate largestDistance{L}(integer N, int *x, integer d) = 
	distance(N, x, d) && 
	(\forall integer dis; distance(N, x, dis) ==> dis <= d);
*/

/*@ requires N >= 2 && N <= MAXN;
    requires \valid(x+ (0..N-1));
    requires \forall integer i; 0 <= i < N ==> x[i] > 0 && x[i] <= MAXV;
    assigns \nothing;

    behavior zero:
    	requires \forall integer i; 0 <= i < N ==>
    		\forall integer j; 0 <= j < i ==> x[i] != x[j];
    	ensures \result == 0;

    behavior non_zero:
    	requires \exists integer i; 0 <= i < N &&
    		\exists integer j; 0 <= j < i && x[i] == x[j];
    	ensures \result != 0 && largestDistance(N, x, \result);

    disjoint behaviors;
	complete behaviors;
*/
int samenum(int N, int *x) {
	int p[MAXV+1];
	/*@ loop invariant 0 <= i <= MAXV + 1;
		loop invariant \forall integer j; 0 <= j < i ==> p[j] == -1;
		loop assigns i, p[0 .. MAXV];
		loop variant MAXV + 1 - i;
	*/
	for (int i = 0; i <= MAXV; ++i) p[i] = -1;
	int best = 0;
	/*@ loop invariant 0 <= i <= N;
        loop invariant \forall integer j; 0 <= j < N ==> p[x[j]] >= -1;
        loop invariant best == 0 || largestDistance(i, x, best);
        loop invariant \forall integer j; 0 <= j < i && \at(p[x[j]], Pre) == -1
			==> p[x[j]] == j;
		loop invariant \forall integer j; 0 <= j < i && \at(p[x[j]], Pre) != -1
			==> p[x[j]] == \at(p[x[j]], Pre);
		loop assigns i, p[0 .. MAXV], best;
		loop variant N - i;
	*/
	for (int i = 0; i < N; ++i)
		if (p[x[i]] == -1) p[x[i]] = i;
		else if (i-p[x[i]] > best) best = i-p[x[i]];
	return best;
}
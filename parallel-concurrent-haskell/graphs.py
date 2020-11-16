from timeit import default_timer as timer
import subprocess, os
import matplotlib.pyplot as plt

sizes = [5]
for _ in range(9):
	sizes.append(sizes[-1]*2)

for N in [2, 4, 8]:
	par_speedups = []
	conc_speedups = []
	for size in sizes:
		times = []
		for prog in ["./sequential", "./parallel2", "./concurrent"]:
			procs = "-N{}".format(N) if not "seq" in prog else ""
			inp = open("testcase_{}.txt".format(size), 'r')
			out = open(os.devnull, 'w')
			command = prog + " +RTS " + procs

			start = timer()
			subprocess.call(command, shell=True, stdin=inp, stdout=out)
			end = timer()
			times.append(end - start)
		par_speedups.append(times[0] / times[1])
		conc_speedups.append(times[0] / times[2])
	plt.figure(figsize=(10,10))
	plt.plot(sizes, par_speedups, label='Parallel')
	plt.plot(sizes, conc_speedups, label='Concurrent')
	plt.xlabel("Testcase size (difficulty may vary)")
	plt.ylabel("Speedup")
	plt.legend()
	plt.title('Running on ' + str(N) + 'processors')
	plt.savefig(str(N) + '_speedup')
	plt.close
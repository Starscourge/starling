# The same program as the VM's fib benchmark, as a point of reference.
# Run with: python3 bench/fib.py

import time


def fib(n):
    return n if n < 2 else fib(n - 1) + fib(n - 2)


times = []
for _ in range(7):
    start = time.perf_counter()
    fib(32)
    times.append((time.perf_counter() - start) * 1000)

print(f"fib 32: {sorted(times)[3]:.2f} ms (median of 7)")

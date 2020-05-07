#benchmarks = {"incremental push/pop unknowns" : "bench-incu.txt", "incremental": "bench-inc.txt", "naive set":"bench-naive.txt"}
benchmarks = {"incremental push/pop unknowns" : "bench-incu.txt", "incremental": "bench-inc.txt"}
#benchmarks = {"sample" : "bench-sample.txt"}

testing_offset = len("Testing ")

all_names = []
all_headers = []
all_maps = {} # name -> map
for bench_header,bench_file in benchmarks.iteritems():
    names = []
    m = {}
    cur_name = None
    with open (bench_file) as f:
        for line in f:
            if line.startswith("Testing "):
                cur_name = line[testing_offset:].strip()
                if cur_name not in all_names:
                    all_names.append(cur_name)
            elif "cpu time" in line:
                data = line[0:line.index('s')].strip()
                m[cur_name] = data
            elif "real time" in line:
                data = line[0:line.index('s')].strip()
                m[cur_name] += "," + data
    all_headers.append(bench_header)
    all_maps[bench_header] = m


print(" "),
for header in all_headers:
    print(",%s (cpu),%s (real)" % (header, header)),
print("")

for name in all_names:
    res = []
    res.append(name)
    for header in all_headers:
        res.append(all_maps[header][name])
    print(",".join(res))


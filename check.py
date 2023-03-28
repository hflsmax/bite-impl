#!/usr/bin/env python3
import argparse
import difflib
import json
import os
import subprocess
import tempfile
from tabulate import tabulate


def parse_args():
    parser = argparse.ArgumentParser(description="Run Bite compiler examples")
    parser.add_argument("example", nargs="*", help="Examples to run. If not specified, run all examples.")
    parser.add_argument("--perf", action="store_true", help="Run examples with hyperfine and report performance results.")
    parser.add_argument("--save", action="store_true", help="Save performance and output data to file.")
    return parser.parse_args()


def get_examples():
    return [os.path.splitext(f)[0] for f in os.listdir("./examples") if f.endswith(".bite")]


def run_example(name):
    print(f"Running example {name}")
    subprocess.run(["dune", "exec", "--display", "quiet", "bite", "--", "-l", f"./examples/{name}.bite", "-o", f"./examples/{name}.compiled.c", "-oir", f"./examples/{name}.ir"], check=True)
    subprocess.run(["clang-format", "-i", f"./examples/{name}.compiled.c"], check=True)
    subprocess.run(["clang", "-O3", "-o", f"./examples/{name}.exe", f"./examples/{name}.compiled.c"], check=True)
    output = subprocess.run([f"./examples/{name}.exe"], stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=True, text=True)
    return output.stdout


def save_data(perf_data):
    if args.save:
        with open("./examples/perf.json", "w") as f:
            json.dump(perf_data, f, indent=4)


def load_data():
    perf_data = {}
    if os.path.isfile("./examples/perf.json"):
        with open("./examples/perf.json", "r") as f:
            perf_data = json.load(f)
    return perf_data


def run_perf(name):
    output = run_example(name)
    with tempfile.NamedTemporaryFile() as tmp:
        subprocess.run(["hyperfine", "--shell=none", "-w", "3", "--export-json", tmp.name, f"./examples/{name}.exe", "--"], check=True, text=True)
        perf_json = json.load(tmp)
        if perf_json:
            mean = perf_json["results"][0]["mean"] * 1000
            stddev = perf_json["results"][0]["stddev"] * 1000
            user = perf_json["results"][0]["user"] * 1000
            system = perf_json["results"][0]["system"] * 1000
            return name, mean, stddev, user, system, output
        else:
            print(f"{name} failed to run with hyperfine")


args = parse_args()

if args.example:
    examples = args.example
else:
    examples = get_examples()

perf_results = []

if args.perf:
    print("Running performance tests...")
    print()
    for name in examples:
        result = run_perf(name)
        if result:
            perf_results.append(result)

    if perf_results:
        loaded_data = load_data()
        print("Performance results:")
        print()
        headers = ["Example", "Mean (ms)", "Stddev (ms)", "User (ms)", "System (ms)", "Change (%)", "Output"]
        table = []
        for name, mean, stddev, user, system, output in perf_results:
            mean_change = "NA"
            if name in loaded_data:
                if loaded_data[name]["output"] != output:
                    output = f"{output} (DIFFERENT)"
                mean_change = (mean - loaded_data[name]["mean"]) / loaded_data[name]["mean"] * 100
            table.append([name, mean, stddev, user, system, mean_change, output])
        print(tabulate(table, headers=headers, floatfmt=".2f"))
        print()

    if args.save:
        print("Saving data...")
        print()
        perf_data = {}
        for name, mean, stddev, user, system, output in perf_results:
            data = {
                "mean": mean,
                "stddev": stddev,
                "user": user,
                "system": system,
                "output": output,
            }
            perf_data[name] = data
        save_data(perf_data)
else:
    print("Checking examples...")
    print()
    loaded_data = load_data()
    for name in examples:
        output_data = run_example(name).strip()
        print(f"{output_data}")
        if name in loaded_data and loaded_data[name]["output"] != output:
            print(f"Output for {name} is different!")

print("Check completed successfully!")

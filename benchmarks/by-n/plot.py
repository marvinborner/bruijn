#!/bin/env python3

import matplotlib.pyplot as plt
import json
import os, glob


def render(file):
    data = json.load(open(file, "r"))["results"]
    basename = os.path.basename(file).split(".")[0]
    reducers = list({p["parameters"]["REDUCER"] for p in data})
    for reducer in reducers:
        labels = [
            p["parameters"]["N"]
            for p in data
            if p["parameters"]["REDUCER"] == reducer
        ]
        times = [
            p["median"] for p in data if p["parameters"]["REDUCER"] == reducer
        ]
        plt.plot(times)
        plt.xticks(range(len(labels)), labels)
    plt.legend(reducers)
    plt.title(f"reducer comparison for {basename}")
    plt.xlabel("N")
    plt.ylabel("Time (s)")
    plt.savefig(f"{basename}.png")
    plt.close()


for file in glob.glob("*.json"):
    render(file)

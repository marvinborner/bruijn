#!/bin/env python

from pathlib import Path
import json


def list_defs(path, kind, prefix):
    res = []
    for line in open(path).readlines():
        if line.startswith(":input"):
            parts = line.split(" ")
            input_path = parts[1].strip().split("std/")[1] + ".bruijn"
            # not using kind="input" is important, "import"s should be inherited
            res = res + list_defs(input_path, kind, prefix)
        elif line.startswith(":import") and kind == "input":
            parts = line.split(" ")
            import_path = parts[1].strip().split("std/")[1] + ".bruijn"
            new_prefix = (
                "" if parts[2].strip() == "." else parts[2].strip() + "."
            )
            new_prefix = prefix + new_prefix if prefix else new_prefix
            res = res + list_defs(import_path, "import", new_prefix)
        elif (
            line.startswith(":")
            or line.startswith("#")
            or line.strip() == ""
            or line[0].isspace()
        ):
            continue
        else:
            parts = line.strip().split(" ")
            res.append(
                {
                    "name": prefix + parts[0],
                    "source": path,
                    "kind": kind,
                }
            )
    return res


res = {}
files = Path(".").glob("**/*.bruijn")
for file in files:
    path = str(file)
    if path != "All.bruijn" and "Generic" not in path:
        res[path] = list_defs(path, "input", "")
print(json.dumps(res))

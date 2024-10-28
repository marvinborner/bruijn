// MIT License, Copyright (c) 2024 Marvin Borner
// high-quality syntax highlighter and intelligence
console.warn(
  "bruijnIntelligence is still WIP, warnings are not representative",
);

// i hate web dev
const decodeHTML = (html) => {
  const txt = document.createElement("textarea");
  txt.innerHTML = html;
  return txt.value;
};

const encodeHTML = (str) =>
  str.replace(/[\u00A0-\u9999<>\&\#]/g, (i) => "&#" + i.charCodeAt(0) + ";");

// just basic token parsing
const parseTerm = (str) => {
  const tokens = [];
  const matchers = {
    whitespace: /^(\s+)(.*)/,
    type: /^(⧗ .*)(.*)$/,
    string: /^("[^\"]*")(.*)/,
    char: /^('[^\']*')(.*)/,
    complex: /^(\([+-][0-9]+\.[0-9]+[+-][0-9]+\.[0-9]+i\))(.*)/,
    float: /^(\([+-][0-9]+\.[0-9]+[qr]?\))(.*)/,
    number: /^(\([+-][0-9]+[ubtd]?\))(.*)/,
    index: /^([0-9])(.*)/,
    abstraction: /^([\[\]])(.*)/,
    application: /^([\(\)])(.*)/,
    namespace: /^([Α-ΩA-Z]+[^\s\[\]\(\)\.]*)(.*)/,
    identifier: /^([α-ωa-z]+[^\s\[\]\(\)\.]*)(.*)/,
    special: /^([^0-9Α-ΩA-Zα-ωa-z\s\(\)\[\]"']+)(.*)/,
  };

  while (str) {
    let matches;
    for ([kind, matcher] of Object.entries(matchers)) {
      if ((matches = str.match(matcher))) {
        tokens.push({ kind, string: matches[1] });
        break;
      }
    }
    if (!matches) {
      console.warn("unknown term", str.slice(0, 10));
      break;
    }
    str = matches[2];
  }
  return tokens;
};

const parseFile = (str) => {
  const tree = [];
  str.split("\n").forEach((line) => {
    let matches;
    if ((matches = line.match(/^:input (.*)/))) {
      tree.push({ kind: "input", path: matches[1] });
    } else if ((matches = line.match(/^:import (.*) (.*)$/))) {
      tree.push({ kind: "import", path: matches[1], namespace: matches[2] });
    } else if ((matches = line.match(/^:import (.*)$/))) {
      const namespace = matches[1].split("/").slice(-1)[0];
      tree.push({ kind: "import", path: matches[1], namespace: namespace });
    } else if ((matches = line.match(/^:test (\(.*\)) (\(.*\))$/))) {
      tree.push({
        kind: "test",
        left: parseTerm(matches[1]),
        right: parseTerm(matches[2]),
      });
    } else if ((matches = line.match(/^:time (.*)$/))) {
      tree.push({ kind: "time", term: parseTerm(matches[1]) });
    } else if ((matches = line.match(/^([ \t]*[^:\n#][^ ]*) (.*)$/))) {
      tree.push({
        kind: "definition",
        name: matches[1],
        term: parseTerm(matches[2]),
      });
    } else if ((matches = line.match(/^# (.*)$/))) {
      tree.push({ kind: "comment", string: matches[1] });
    } else if ((matches = line.match(/(\s)+/))) {
      tree.push({ kind: "whitespace", string: matches[1] });
    } else if (line === "") {
      tree.push({ kind: "whitespace", string: line });
    } else {
      console.warn("unknown line", line);
    }
  });
  return tree;
};

const dumpTree = (tree) => {
  const dumpTerm = (tokens) =>
    tokens
      .map(
        (token) =>
          `<span class="${token.kind}">${encodeHTML(token.string)}</span>`,
      )
      .join("");

  return tree
    .map((line) => {
      switch (line.kind) {
        case "input":
          return `<span class="pp input">:input ${line.path}</span>`;
        case "import":
          return `<span class="pp import">:import ${line.path} ${line.namespace}</span>`;
        case "test":
          return `<span class="pp test">:test</span> ${dumpTerm(line.left)} ${dumpTerm(line.right)}`;
        case "time":
          return `<span class="pp time">:time</span> ${dumpTerm(line.term)}`;
        case "definition":
          return `<span class="definition">${encodeHTML(line.name)}</span> ${dumpTerm(line.term)}`;
        case "comment":
          return `<span class="comment"># ${encodeHTML(line.string)}</span>`;
        case "whitespace":
          return line.string;
        case "verbatim":
          return dumpTerm(line.tokens);
        default:
          console.error("unknown kind", line.kind);
          return "";
      }
    })
    .join("\n");
};

// TODO: this is fine for medium files but not large programs
const highlight = (elem) => {
  const tree = parseFile(decodeHTML(elem.innerHTML));
  elem.innerHTML = dumpTree(tree);
};

const highlightTerm = (elem) => {
  const tokens = parseTerm(decodeHTML(elem.innerHTML));
  elem.innerHTML = dumpTree([{ kind: "verbatim", tokens }]);
};

/**
 * It might seem dumb that we parse everything again from DOM.
 * You're right. However, we use a very similar structure in blog
 * so it's much easier to recycle the code here.
 * TODO: eventually merge this with blog
 */

const bruijnScroll = (elem) => (f) => elem.scrollIntoView();

const bruijnParse = (stdMap) => {
  const tokens = [
    ...document.querySelectorAll("code.language-bruijn span"),
  ].filter((t) => !!t.classList.length);
  const root = { indent: -1, children: [], nodes: [], name: "<root>" };
  const stack = [root];
  for (let i = 0; i < tokens.length; i++) {
    const token = tokens[i];
    const current = stack[stack.length - 1];
    const inner = token.innerHTML;
    const tabIndent = inner[0] === "\t";
    const indent =
      token.classList[0] === "definition"
        ? tabIndent
          ? (inner.match(/[\t]/g) ?? "").length
          : (inner.match(/[\s]/g) ?? "").length / 4
        : 0;
    const fresh = {
      kind: "definition",
      elem: token,
      indent,
      children: [],
      nodes: [],
      name: inner.trim(),
      action: bruijnScroll(token),
    };

    if (token.classList[0] === "definition") {
      // definition
      if (indent < current.indent) {
        // indent left
        for (let j = 0; j < current.indent - indent + 1; j++) stack.pop();
        stack[stack.length - 1].children.push(fresh);
        stack.push(fresh);
      } else if (indent > current.indent) {
        // indent right
        current.children.push(fresh);
        stack.push(fresh);
      } else {
        // same indent
        stack.pop();
        stack[stack.length - 1].children.push(fresh);
        stack.push(fresh);
      }
    } else if (token.classList[0] === "pp") {
      // preprocessor
      const instr = inner.trim();
      const isImport = instr.startsWith(":import");
      const isInput = instr.startsWith(":input");
      if (isImport || isInput) {
        const parts = instr.split(" ");
        const path = parts[1].replace("std/", "") + ".bruijn";
        if (!(path in stdMap)) {
          console.warn("invalid import", path);
          continue;
        }

        stdMap[path].forEach((d) => {
          const name = isInput
            ? d.name
            : parts[2] == "."
              ? d.name
              : parts[2] + "." + d.name;
          const link = d.source.replace("/", "_");
          const original = path.replace("/", "_");
          if (isInput || (isImport && d.kind !== "import"))
            root.children.push({
              kind: "import",
              elem: token,
              indent: 0,
              action: (f) =>
                window.open(
                  `https://bruijn.marvinborner.de/std/${f == "" ? original : link}.html#${f}`,
                  "_blank",
                ),
              children: [],
              nodes: [],
              name,
            });
        });
      } else if (instr.startsWith(":test") || instr.startsWith(":time")) {
        root.children.push(fresh);
      }
    }

    // just before expression: definition/preprocessor
    if (["definition", "pp"].includes(token.classList[0])) {
      const bruijnStack = [];
      for (
        let j = i + 1;
        j < tokens.length &&
        !["definition", "pp"].includes(tokens[j].classList[0]);
        j++
      ) {
        if (
          j > 0 &&
          tokens[j - 1].classList[0] === "namespace" &&
          tokens[j].classList[0] === "special" &&
          tokens[j].innerHTML.trim().startsWith(".") &&
          tokens[j].innerHTML.trim().length !== 1 // && something next != space TODO
        ) {
          // namespaced prefix
          const decoded = decodeHTML(tokens[j].innerHTML.trim());
          const prefixed =
            decoded[decoded.length - 1] === "‣" ? decoded : decoded + "‣";
          fresh.nodes.push({
            kind: "prefix",
            elem: tokens[j],
            name: tokens[j - 1].innerHTML.trim() + prefixed,
            bruijnStack: [...bruijnStack], // for subs
            canonical: prefixed.slice(1),
          });
        } else if (
          tokens[j].classList[0] === "special" &&
          tokens[j].innerHTML.trim() !== "." // && something next != space TODO
        ) {
          // normal prefix
          const decoded = decodeHTML(tokens[j].innerHTML.trim());
          const prefixed =
            decoded[decoded.length - 1] === "‣" ? decoded : decoded + "‣";
          fresh.nodes.push({
            kind: "prefix",
            elem: tokens[j],
            name: prefixed,
            bruijnStack: [...bruijnStack], // for subs
            canonical: prefixed,
          });

          // normal mixfix TODO: this is a ugly hack and only works if lucky and binary
          const mixfixed = decoded[decoded.length - 1].includes("…")
            ? decoded
            : "…" + decoded + "…";
          fresh.nodes.push({
            kind: "mixfix",
            elem: tokens[j],
            name: mixfixed,
            bruijnStack: [...bruijnStack], // for subs
            canonical: mixfixed,
          });
        } else if (
          j > 1 &&
          tokens[j - 2].classList[0] === "namespace" &&
          tokens[j - 1].classList[0] === "special" &&
          tokens[j - 1].innerHTML.trim() === "." &&
          tokens[j].classList[0] === "identifier"
        ) {
          // namespaced identifier
          fresh.nodes.push({
            kind: "identifier",
            elem: tokens[j],
            name:
              tokens[j - 2].innerHTML.trim() + "." + tokens[j].innerHTML.trim(),
            bruijnStack: [...bruijnStack], // for subs
            canonical: tokens[j].innerHTML.trim(),
          });
        } else if (tokens[j].classList[0] === "identifier") {
          // normal identifier
          fresh.nodes.push({
            kind: "identifier",
            elem: tokens[j],
            name: tokens[j].innerHTML.trim(),
            bruijnStack: [...bruijnStack], // for subs
            canonical: tokens[j].innerHTML.trim(),
          });
        } else if (tokens[j].classList[0] === "abstraction") {
          // square bracket / abstraction
          const inner = tokens[j].innerHTML.trim();
          if (inner === "[")
            bruijnStack.push({
              kind: "abstraction",
              elem: tokens[j],
              indent: 0,
              action: (f) => {},
              children: [],
              nodes: [],
            });
          else if (inner === "]") {
            const left = bruijnStack.pop();
            left.right = tokens[j];
          }
        } else if (tokens[j].classList[0] === "index") {
          // de Bruijn index
          const inner = tokens[j].innerHTML.trim();
          if (inner.length !== 1 || !(inner[0] >= "0" && inner[0] <= "9")) {
            console.warn("weird de Bruijn index", inner);
            continue;
          }
          const left = bruijnStack[bruijnStack.length - 1 - +inner];
          fresh.nodes.push({
            kind: "index",
            elem: tokens[j],
            target: left,
            right: left ? left.right : left,
          });

          // also highlight abstractions in sup terms
          const walkUp = [...stack];
          walkUp.shift(); // without root
          walkUp.pop(); // without current
          let metaStacks = {};
          metaStacks[stack[stack.length - 1].name] = [
            [...bruijnStack].reverse(),
          ];
          while (walkUp.length > 0) {
            const sup = walkUp.pop();
            sup.nodes
              .filter((node) => node.name in metaStacks)
              .forEach((node) => {
                const metaStack = [];
                metaStacks[node.name].forEach((meta) => {
                  metaStack.push(
                    [...meta].concat([...node.bruijnStack].reverse()),
                  );
                });

                metaStack.forEach((meta) => {
                  const upperLeft = meta[+inner];
                  if (!upperLeft) {
                    // console.warn(sup.name, inner, sup.elem, node.elem); // TODO: fix??
                  } else
                    fresh.nodes.push({
                      kind: "index",
                      elem: tokens[j],
                      target: upperLeft,
                      right: upperLeft ? upperLeft.right : upperLeft,
                    });
                });

                metaStacks[sup.name] = metaStack;
              });
          }

          // TODO: case is missing:
          // foo [bar]
          //     baz [0 1 2] # won't get highlighted
          //     bar [baz]
        }
      }
    }
  }
  return root;
};

const bruijnApply = (root) => {
  const crumbs = [0]; // unintended identity
  while (crumbs.length > 0) {
    // TODO: this makes it really slow and isn't necessary
    const index = (indices, obj) => {
      const head = indices[0];
      const tail = indices.slice(1);
      if (!obj || head >= obj.children.length) return undefined;
      else if (tail.length > 0) return index(tail, obj.children[head]);
      else return obj.children[head];
    };
    const current = index(crumbs, root);
    if (!current) {
      crumbs.pop();
      if (crumbs.length > 0) ++crumbs[crumbs.length - 1];
      continue;
    }
    current.nodes.forEach((node) => {
      let res;

      if (node.kind === "index") res = node.target;

      // first search in sub
      // this only nests 1 deep, see previous versions for DFS
      // e.g. β* was buggy with this though, we'd need BFS
      let _crumbs = [...crumbs, 0];
      while (!res && _crumbs.length > 0) {
        const found = index(_crumbs, root);
        if (!found) break;
        if (found.name === node.name) {
          res = found;
          break;
        }
        ++_crumbs[_crumbs.length - 1];
      }

      // ...then in sup
      _crumbs = [...crumbs];
      // skip itself
      _crumbs[_crumbs.length - 1] -= _crumbs[_crumbs.length - 1] == 0 ? 0 : 1;
      while (!res && _crumbs.length > 0) {
        const found = index(_crumbs, root);
        if (!found) {
          console.error("wtf lol what happened");
          break;
        }
        if (found.name === node.name) {
          res = found;
          break;
        }

        if (_crumbs[_crumbs.length - 1] === 0) _crumbs.pop();
        else --_crumbs[_crumbs.length - 1];
      }

      if (res) {
        // densely connected listeners
        node.elem.addEventListener(
          "click",
          (ev) => {
            res.action(node.canonical);
            ev.stopImmediatePropagation();
          },
          true,
        );
        res.elem.addEventListener(
          "click",
          (ev) => {
            res.action("");
            ev.stopImmediatePropagation();
          },
          true,
        );
        const connectors = [node.elem, res.elem];
        if ("right" in res) connectors.push(res.right); // right abstraction
        connectors.forEach((elem) => {
          elem.addEventListener("mouseenter", () => {
            node.elem.classList.add("code-highlight");
            connectors.forEach((elem2) =>
              elem2.classList.add("code-highlight"),
            );
          });
          elem.addEventListener("mouseout", () => {
            node.elem.classList.remove("code-highlight");
            connectors.forEach((elem2) =>
              elem2.classList.remove("code-highlight"),
            );
          });
          elem.classList.add("code-clickable");
        });
        if (["definition", "import"].includes(res.kind)) {
          node.elem.title = `Jump to ${res.kind} source`;
        }
      } else if (node.kind === "identifier") {
        // TODO: also include prefix/mixfix once better
        console.warn("unbound", node.kind, node.name);
      }
    });

    // try to nest deeper
    crumbs.push(0);
  }
};

const becomeIntelligent = () => {
  fetch("https://bruijn.marvinborner.de/std_map.json")
    .then((resp) => {
      return resp.json();
    })
    .then((data) => {
      [...document.querySelectorAll("code.language-bruijn")].forEach(highlight);
      [...document.querySelectorAll("code.bruijn")].forEach(highlightTerm);

      const tree = bruijnParse(data);
      bruijnApply(tree);

      // jump to given function
      const hash = document.location.hash;
      if (hash) {
        const definitions = document.querySelectorAll(
          "code.language-bruijn span.definition",
        );
        definitions.forEach((definition) => {
          if (definition.innerText === decodeURI(hash).slice(1))
            bruijnScroll(definition)();
        });
      }
    })
    .catch((err) => {
      console.error(err);
    });
};

// onload needed for wiki
window.onload = () => {
  if (document.querySelector("code.language-bruijn, code.bruijn"))
    becomeIntelligent();
};

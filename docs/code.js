// high-quality syntax highlighter
// TODO: Implement actual parser (or fix MANY regex bugs)
// TODO: bug: -0 in (-0 0) is not highlighted as index

const term = (t) =>
  t
    .replaceAll(
      /(?<!\([+-]\d*\.?\d*\+?\d*\.?\d*)(?<![a-z][^&; ]*)(?<!["'])([0-9])/g,
      "<span class='index'>$1</span>",
    )
    .replaceAll(/'([^\'])'/g, "<span class='string'>'$1'</span>")
    .replaceAll(/"([^\"]*)"/g, "<span class='string'>\"$1\"</span>")
    .replaceAll(
      /(\([+-][0-9]+\.[0-9]+[+-][0-9]+\.[0-9]+i\))/g,
      "<span class='number'>$1</span>",
    )
    .replaceAll(
      /(\([+-][0-9]+\.[0-9]+[qr]?\))/g,
      "<span class='number'>$1</span>",
    )
    .replaceAll(/(\([+-][0-9]+[ubtd]?\))/g, "<span class='number'>$1</span>")
    .replaceAll(/(?<!\>)(\()/g, "<span class='left-app'>(</span>")
    .replaceAll(/(\))(?!\<)/g, "<span class='right-app'>)</span>")
    .replaceAll("[", "<span class='left-abs'>[</span>")
    .replaceAll("]", "<span class='right-abs'>]</span>");

const highlightTerm = (elem) => {
  elem.innerHTML = term(elem.innerHTML);
};

const highlight = (elem) => {
  const fixPath = (p) => p.replace("/", "_");

  elem.innerHTML = elem.innerHTML
    .replaceAll(
      /^:import std\/(.*) (.*)$/gm,
      (_, p, s) =>
        `<span class="com">:import</span> <a href='/std/${fixPath(
          p,
        )}.bruijn.html'>std/${p}</a> ${s}`,
    )
    .replaceAll(
      /^:input std\/(.*)$/gm,
      (_, p) =>
        `<span class="com">:input</span> <a href='/std/${fixPath(
          p,
        )}.bruijn.html'>std/${p}</a>`,
    )
    .replaceAll(
      /^:import (.*) (.*)$/gm,
      (_, p, s) => `<span class="com">:import</span> ${p} ${s}`,
    )
    .replaceAll(
      /^:test (\(.*\)) (\(.*\))$/gm,
      (_, t1, t2) => `<span class='com'>:test</span> ${term(t1)} ${term(t2)}`,
    )
    .replaceAll(
      /^:time (.*)$/gm,
      (_, t) => `<span class='com'>:time</span> ${term(t)}`,
    )
    .replaceAll(
      /^([ \t]*[^:\n<#][^ ]*) (.*)$/gm,
      (_, d, t) => `<span class='def'>${d}</span> ${term(t)}`,
    )
    .replaceAll(/^# (.*)$/gm, "<span class='comment'># $1</span>")
    .replaceAll(/ ⧗ (.*)\n/g, " ⧗ <span class='type'>$1</span>\n");
};

window.onload = () => {
  [...document.querySelectorAll("code.language-bruijn")].forEach(highlight);
  [...document.querySelectorAll("code.bruijn")].forEach(highlightTerm);
};

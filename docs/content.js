const code = document.getElementsByTagName("pre")[0]

const fixPath = p => p.replace("/", "_")

const term = t => t
	.replaceAll(/(\([+-][0-9]+[ubt]?\))/g, "<span class='number'>$1</span>")
	.replaceAll(/(?<!\>)(\()/g, "<span class='left-app'>(</span>")
	.replaceAll(/(\))(?!\<)/g, "<span class='right-app'>)</span>")
	.replaceAll("[", "<span class='left-abs'>[</span>")
	.replaceAll("]", "<span class='right-abs'>]</span>")
	.replaceAll(/(?<![+-\d])([0-9])/g, "<span class='index'>$1</span>")

code.innerHTML = code.innerHTML
	.replaceAll(/^:import std\/(.*) (.*)$/gm, (_, p, s) => `<span class="com">:import</span> <a href='${fixPath(p)}.bruijn.html'>std/${p}</a> ${s}`)
	.replaceAll(/^:input std\/(.*)$/gm, (_, p) => `<span class="com">:input</span> <a href='${fixPath(p)}.bruijn.html'>std/${p}</a>`)
	.replaceAll(/^:test \((.*)\) \((.*)\)$/gm, (_, t1, t2) => `<span class='com'>:test</span> (${term(t1)}) (${term(t2)})`)
	.replaceAll(/^([^:\n<#][^ ]*) (.*)$/gm, (_, d, t) => `<span class='def'>${d}</span> ${term(t)}`)
	.replaceAll(/^# (.*)$/gm, "<span class='comment'># $1</span>")
	.replaceAll(/ ⧗ (.*)\n/g, " ⧗ <span class='type'>$1</span>\n")

code.innerHTML = `<span class="line"></span>${code.innerHTML}<span class="cl"></span>`
const lines = code.innerHTML.split(/\n/).length - 1
for (let i = 0; i < lines; i++) {
	const cur = code.getElementsByTagName("span")[0]
	cur.innerHTML += `<span>${i + 1}</span>`
}

[...document.getElementsByClassName("term")].forEach(el => {
	el.innerHTML = el.innerHTML
		.replaceAll(/(?<!\>)(\()/g, "<span class='left-app'>(</span>")
		.replaceAll(/(\))(?!\<)/g, "<span class='right-app'>)</span>")
		.replaceAll("[", "<span class='left-abs'>[</span>")
		.replaceAll("]", "<span class='right-abs'>]</span>")
		.replaceAll(/(?<!\+)([0-9])/g, "<span class='index'>$1</span>")
})

const clearPopups = () => {
	[...document.getElementsByClassName("popup")].forEach(el => {
		el.remove()
	})
}

const notify = (s, e) => {
	console.log(e);
	clearPopups()
	const popup = document.createElement("div")
	popup.className = "popup"
	const content = document.createTextNode(s)
	popup.style.left = e.pageX + "px";
	popup.style.top = e.pageY + "px";
	popup.appendChild(content)
	document.body.appendChild(popup)
}

const describe = (c, d) => {
	[...document.getElementsByClassName(c)].forEach(el => el.addEventListener("click", e => notify(d, e)));
}

describe("binary", "Syntactic sugar for a binary number representation using abstractions as data. Needs a sign and brackets to differentiate it from bruijn indices");
describe("char", "Syntactic sugar for a binary representation of characters using abstractions as data.");
describe("com", "This indicates a command to the interpreter. The most common commands are :test (verifying α-equivalency) and :import (importing definitions from other files).");
describe("def", "This defines a new term substitution.");
describe("header", "[0] is the identity operation. It returns the first argument it gets. Nothing more.");
describe("index", "These numbers reference the nth abstraction, starting counting from the inside. These 'bruijn indices' replace the concept of variables in lambda calculus.");
describe("left-abs", "The opening bracket of a function abstraction. It's basically the equivalent of the λ in lambda calculus.");
describe("left-app", "The opening bracket of a function application.");
describe("mixfix", "This is a mixfix operator. They can be defined like …*… where the … can then be any other term. You can use them without the … as a notation of function application.");
describe("repl", "This indicates a REPL input.");
describe("right-abs", "The closing bracket of a function abstraction.");
describe("right-app", "The closing bracket of a function application.");
describe("stack", "Stack is a dependency manager for Haskell. Install it using the corresponding instructions for your operating system.")
describe("string", "Syntactic sugar for a list of binary encoded chars.")
describe("symbol", "This substitutes a previously defined term (for example from the standard library).");
describe("ternary", "Syntactic sugar for a balanced ternary number representation using abstractions as data. Needs a sign and brackets to differentiate it from bruijn indices.");
describe("time", "Incredibly fast for lambda calculus standards.");

document.body.addEventListener("click", clearPopups, true)

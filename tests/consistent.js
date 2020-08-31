class counter {
	constructor() {
		this.counted = new WatchablePrimitive(0);
	}
	render() {
		return el("div", [
			attr("class", "counter"),
			button(["++"]).onev("click", () => this.counted.set(this.counted.value + 1)),
			span([" ", tn().dwth(v => this.counted.watch(() => v.nodeValue = "" + this.counted.value)), " "]),
			button(["--"]).onev("click", () => this.counted.set(this.counted.value + 1)),
		]);
	}
}

const counter_0 = () => {
	const $counted_1 = new ō.WatchablePrimitive(0);
	return ō.el("div", [
		ō.attr("class", "counter"),
		ō.el("button", ["++"]).onev("click", () => $counted_1.set($counted_1.value + 1)),
		ō.el("span", [" ", $counted_1, " "]),
	]);
}

var counter_0 = () => {
    var $counted_2 = ō.watchable(0);
    return ō.html("div",
        ō.attr("class", "counter"),
        ō.html("span",
            " ",
            $counted_2,
            " "
        )
    );
};

class counter_0 {
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

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
        ō.html("button",
            "++",
            ō.attr("onclick", () => {
                var _7_ = ($counted_2).value;
                var _6_ = _7_ + 1;
                ($counted_2).set(_6_);
                var _5_ = undefined;
                return _5_;
            })
        ),
        ō.html("span",
            " ",
            $counted_2,
            " "
        ),
        ō.html("button",
            "--",
            ō.attr("onclick", () => {
                var _10_ = ($counted_2).value;
                var _9_ = _10_ - 1;
                ($counted_2).set(_9_);
                var _8_ = undefined;
                return _8_;
            })
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

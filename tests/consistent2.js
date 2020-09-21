const counter_0 = () => {
	const $counted_1 = new ō.WatchablePrimitive(0);
	return ō.el("div", [
		ō.attr("class", "counter"),
		ō.el("button", ["++"]).onev("click", () => $counted_1.set($counted_1.value + 1)),
		ō.el("span", [" ", $counted_1, " "]),
	]);
}

// so this isn't quite right yet because::
// $counted_2's type is not watchable
// it should be ō.watch(v => v, [$counted_2])
// but it's not
// actually false it shouldn't be that uuh
// it should only be that if $counted_2 + 1
// eg `ō.watch(v => v + 1, [$counted_2])`
// but there's no reason to make a watchable out of a watchable variable so uuh
var _3_ = () => {
    var _4_ = 0;
    var $counted_2 = ō.watchable(_4_);
    var _8_ = "counter";
    var _8_ = ō.attr("class", _8);
    var _9_ = " ";
    var _10_ = $counted_2;
    var _11_ = " ";
    var _7_ = ō.html("span", _9_, _10_, _11_);
    var _5_ = ō.html("div", _6_, _7_);
    return _5_;
};
var counter_0 = _3_;

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

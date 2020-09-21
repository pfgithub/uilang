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
    var _1_ = undefined;
    _1_blk_: {
        var _5_ = 0;
        var $counted_2 = ō.watchable(_5_);
        var _9_ = "counter";
        var _9_ = ō.attr("class", _9);
        var _10_ = " ";
        var _11_ = $counted_2;
        var _12_ = " ";
        var _8_ = ō.html("span", _10_, _11_, _12_);
        var _6_ = ō.html("div", _7_, _8_);
        _1_ = _6_;
        break _1_blk;
    }
    var _4_ = _1_;
    return _4_;
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

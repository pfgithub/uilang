// Generated code

var counter_0 = () => {
    var $counted_2 = ō.watchable_primitive(0);
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

ō.mount(document.body, counter_0())

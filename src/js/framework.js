// https://github.com/pfgithub/domframework/blob/master/core/src/dom.ts
// https://github.com/pfgithub/domframework/blob/master/core/src/watchable.ts (useful instructions at the top of that)

// type Element = string | 

// in the future, args to html could be coerced into enum(auto){str: string, attr: attr, html: html}
// and then we could just check arg[0] to determine what type it is without messy slow dynamic typing stuff
// eg like instead of ō.html("div", "somestring", ō.attr("a", "b")) it might be eg ō.html("div", [0, "somestring"], [1, ō.attr("a", "b")])

// also in the future this could be rewritten in uil and added to the stdlib so it can easily take advantage of ^

class WatchablePrimitive {
	constructor(initialv) {
		this._watch_list = new Set();
// #if debug
		this._real_value = initialv
// #else
		// this.value = initialv;
// #end
	}
// #if debug
	get value() {
		return this._real_value;
	}
// #end
	set(envy) {
		this._real_value = envy;
		for(let callback of this._watch_list) {
			callback();
		}
	}
	// note this is not exposed to language users
	// 1 + $v gens ō.watch((_1) => 1 + _1, [$v]) eg
	watch(callback) {
		this._watch_list.add(callback);
		return () => this._watch_list.remove(callback);
	}
}
// since we don't have typescript like unions, it should be easy* to
// have watchableprimitive actually only be used for primitives
// so we don't have to worry about what happens if you set
// watchableprimitive to an object because… you can't, enforced by
// the type system (excl. some @typeCast() thing or something)
//
// * nothing is easy once you try to do it except some things

const ō = {
	watchable_primitive(initial) {
		return new WatchablePrimitive(initial);
	},
	
	// reminder that things will be unions in the future
	// it will be easier to do stuff because most things
	// will be statically typed
	html(elname, ...elchildren) {
		return {createBefore(parent, __after_once) {
			const element = document.createElement(elname);
			parent.insertBefore(element, __after_once);
			const watchers = [];
			
			return {removeSelf() {
				element.remove();
				watchers.map(w => w.remove());
			}};
		}};
	},
	
	// attrname: string, attrvalue: (value | Watchable<value>)
	// => AttrSchema
	attr(attrname, attrvalue) {
		
	},
	portal(element, portalto) {
		
	},
	
	// elschema: Element
	mount(elschema, el) {
		
	},
};

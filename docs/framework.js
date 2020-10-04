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
	add_watcher(callback) {
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

const is_existing_node = Symbol("existing node");

const ō = {
	watchable_primitive(initial) {
		return new WatchablePrimitive(initial);
	},
	
	is_existing(spec) {
		return !!spec[is_existing_node];
	},
	is_watchable(spec) {
		return spec instanceof WatchablePrimitive;
	},
	
	// in the future, this will be easy because it will accept a union rather
	// than dynamic typed stuff
	user_element(spec) {
		if(ō.is_existing(spec)) return spec; // already a node
		if(Array.isArray(spec)) return ō.create_fragment_node(spec.map(it => ō.user_element(it)));
		
		let node_has_been_created = false;
		return {[is_existing_node]: true, createBefore(parent, __after_once) {
			if(node_has_been_created) throw new Error("Attempting to createBefore a node that has already been created. TODO what to do?");
			node_has_been_created = true;
			// if watchable
			// oh I forgot this includes ō.attrs too
			// uuh
			if(ō.is_watchable(spec)) {
				// OPTIMIZATION: if prev is text and next is text, just update node.nodeValue
				let node_after = document.createTextNode("");
				parent.insertBefore(node_after, __after_once);
				
				let node_exists = true;
				
				let prev_user_node = undefined;
				let prev_node = undefined;
				let onchange = () => {
					if(!node_exists) {
						// uil should have unreachable "reason" as an expression
						throw new Error("Node updated after removal, even though the watcher was unregistered.");
					}
					// if eq previous value, do nothing
					let new_user_node = spec.value;
					if(prev_user_node === new_user_node) return; // nothing to do;
					prev_user_node = new_user_node;
					// remove existing node
					if(prev_node) prev_node.removeSelf();
					// create real nodes
					if(typeof new_user_node !== "object") {
						node_after.nodeValue = "" + new_user_node;
						prev_node = {removeSelf() {node_after.nodeValue = "";}};
					}else {
						let new_node = ō.user_element(new_user_node);
						prev_node = new_node.createBefore(parent, node_after);
					}
					
					if(window.on_node_update) window.on_node_update(parent);
				};
				let unregister_watcher = spec.add_watcher(onchange);
				onchange();
				
				return {removeSelf() {
					if(prev_node) prev_node.removeSelf();
					unregister_watcher();
					node_exists = false;
				}};
			}
			// once we have unions, this won't be necessary
			if(typeof spec !== "object") {
				let node = document.createTextNode("" + spec);
				parent.insertBefore(node, __after_once);
				return {removeSelf() {node.remove();}};
			}
			console.log("!err", spec);
			throw new Error("Invalid node spec. Unions will make this an unreachable.");
		}}
	},
	
	// reminder that things will be unions in the future
	// it will be easier to do stuff because most things
	// will be statically typed
	html(elname, ...elchildren) {
		return {[is_existing_node]: true, createBefore(parent, __after_once) {
			const thisel = document.createElement(elname);
			parent.insertBefore(thisel, __after_once);
			
			const children = elchildren.map(child => 
				ō.user_element(child).createBefore(thisel, null)
			);
			
			return {removeSelf() {
				children.map(child => child.removeSelf());
				thisel.remove();
			}};
		}};
	},
	
	// attrname: string, attrvalue: (value | Watchable<value>)
	// => AttrSchema
	attr(attrname, attrvalue) {
		return {[is_existing_node]: true, createBefore(parent, __after_once) {
			if(ō.is_watchable(attrvalue)) {
				throw new Error("TODO watchable attrvalue");
			}
			if(attrname.startsWith("on")) {
				if(parent[attrname]) console.log("Potential duplicate attribute", attrname);
				parent[attrname] = attrvalue;
				return {removeSelf() {
					parent[attrname] = undefined;
				}};
			}
			if(parent.hasAttribute(attrname)) console.log("Potential duplicate attribute", attrname);
			if(!attrvalue) parent.removeAttribute(attrname)
			else parent.setAttribute(attrname, "" + attrvalue);
			return {removeSelf() {
				parent.removeAttribute(attrname);
			}};
		}};
	},
	portal(element, portalto) {
		
	},
	
	// elschema: Element
	mount(el, elschema) {
		const unv = ō.user_element(elschema).createBefore(el, null);
		return {unmount() {unv.removeSelf();}};
	},
};

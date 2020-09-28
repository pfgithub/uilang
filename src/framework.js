// https://github.com/pfgithub/domframework/blob/master/core/src/dom.ts
// https://github.com/pfgithub/domframework/blob/master/core/src/watchable.ts (useful instructions at the top of that)

// type Element = string | 

// in the future, args to html could be coerced into enum(auto){str: string, html: html}
// and then we could just check arg[0] to determine what type it is without messy dynamic typing stuff

// also in the future this could be rewritten in uil and added to the stdlib so it can easily take advantage of ^

const ō = {
	// elname: string, elchildren: (Element | Watchable<Element>)[]
	// => ElementSchema
	html(elname, ...elchildren) {
		return {
			// parent: Node, __afterOnce: ?Node
			createBefore(parent, __afterOnce) {
				let nodeAfter = document.createTextNode("");
				parent.insertBefore(nodeAfter, __afterOnce);
				
				return {
					removeSelf: () => {
						nodeAfter.remove();
						// unregister watchers if applicable
					},
				};
			}
		};
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
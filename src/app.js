var app = Elm.Main.init();

function toElm(type, body) {
	app.ports.fromJs.send({
		type: type,
		body: body
	});
}

function square(n) {
	toElm("square computed", n * n);
}

var actions = {
	consoleLog: console.log,
	square: square
}

function jsMsgHandler(msg) {
	var action = actions[msg.type];
	if (typeof action === "undefined") {
		console.log("Unrecognized js msg type ->", msg.type);
		return;
	}
	action(msg.body);
}

//app.ports.toJs.subscribe(jsMsgHandler)

class InfiniteScroller extends HTMLElement {
  constructor() {
    super();

    this.aboveHeight = null;

    this.calculateTopElementsHeight = this.calculateTopElementsHeight.bind(this)
  }

  connectedCallback() {
  }

  calculateTopElementsHeight() {
    console.log("Calculating")
    const pageShiftStr = this.getAttribute("pageShiftSize");
    if (pageShiftStr === null) {
        return
    }

    const pageShiftSize = parseInt(pageShiftStr)

    const rows = []

    let i = 0
    while (i < this.children.length) {
        const el = this.children[i]

        if (el) {
            console.log(el.tagName)
            if (el.tagName === "ROW") {
                rows.push(el)
            }
        }

        i = i + 1
    }

    let height = 0

    i = 0
    const elements = []
    while (i < pageShiftSize) {
        const el = rows[i]
        if (el) {
            const elHeight = el.offsetHeight
            console.log("El in calculation", el.getAttribute("data-label"), el.offsetHeight, el.tagName)
            height = height + elHeight
        }

        i = i + 1
    }

    console.log("Calculated height", height)

    this.aboveHeight = height;
  }

  static get observedAttributes() {
    return ['recalculate', 'shift'];
  }

  attributeChangedCallback(name, oldValue, newValue) {
    if (name === "recalculate" && oldValue !== newValue) {
        setTimeout(this.calculateTopElementsHeight, 0);
    }

    if (name === "shift" && oldValue !== newValue) {
        const payload = JSON.parse(newValue);
        if (payload !== null) {
          if (payload.direction === "down") {
            console.log("Setting scroll")
            console.log("Scroll top", this.scrollTop)
            console.log("Calculated above height", this.aboveHeight)
            this.scrollTop = this.scrollTop - this.aboveHeight

            setTimeout(this.calculateTopElementsHeight, 0);
          }
        }

    }
  }
}

customElements.define("infinite-scroller", InfiniteScroller);

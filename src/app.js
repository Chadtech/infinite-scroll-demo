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

    let height = 0
    let i = 0
    const elements = []
    while (i < (pageShiftSize)) {
        const el = this.children[i]
        if (el) {
            const elHeight = el.offsetHeight
            console.log(elHeight)
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
            console.log(this.scrollTop)
            console.log(this.aboveHeight)
            this.scrollTop = this.scrollTop - this.aboveHeight

            setTimeout(this.calculateTopElementsHeight, 0);
          }
        }

    }
  }
}

customElements.define("infinite-scroller", InfiniteScroller);

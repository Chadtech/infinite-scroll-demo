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
    this.scrollPos = 0
    this.nextAdjustment = null

    this.calculateTopElementsHeight = this.calculateTopElementsHeight.bind(this)
    this.scrolled = this.scrolled.bind(this)
    this.setScroll = this.setScroll.bind(this)

  }

  connectedCallback() {
    this.addEventListener("scroll", this.scrolled)
  }

  setScroll() {
    this.scrollTop = this.scrollPos - this.nextAdjustment
  }

  scrolled(e) {
    this.scrollPos = e.target.scrollTop
  }

  calculateTopElementsHeight(f) {
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
            if (el.tagName === "ROW") {
                rows.push(el)
            }
        }

        i = i + 1
    }

    const nextTopEl = rows[pageShiftSize]

    if (nextTopEl) {
      console.log("Next Top El Pos", nextTopEl.getAttribute("data-label"), nextTopEl.offsetTop)
      this.aboveHeight = nextTopEl.offsetTop
    }

    console.log("Scroll Top in calculate", this.scrollTop)

    if (this.nextAdjustment) {

      this.nextAdjustment = null
    }
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
        if (payload && payload.direction === "down") {
          this.nextAdjustment = 0 + this.aboveHeight
          setTimeout(this.setScroll, 0)
          setTimeout(this.calculateTopElementsHeight, 0)
        }
    }
  }
}

customElements.define("infinite-scroller", InfiniteScroller);

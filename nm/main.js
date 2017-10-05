const readline  = require('readline');
const puppeteer = require('puppeteer');

class Halovi {

  constructor () {
    const args       = process.argv.slice(2);
    const isHeadless = !args.includes('headful');

    this.windows = [];
    (async () => {
      this.browser = await puppeteer.launch({args: ['--no-sandbox'], headless: isHeadless});
      await this.createPage();
      this.output("READY");
    })();
  }

  async createPage () {
    const page = await this.browser.newPage();
    this.windows.push(page);
  }

  output (code = 'SUCCESS', msg = '') {
    console.log(JSON.stringify(
      { respCode: code
      , respMsg: msg }
    ));
  }
  log (msg) { this.output("LOG", msg); }

  activeWindow () { return this.windows[this.windows.length-1]; }


  async run (code) {
    const obj = JSON.parse(code);

    switch (obj.reqCode) {
      case "EXEC":
        await eval(obj.reqMsg)
          . then ( resp => {
            if (resp === undefined)
              this.output();
          })
          . catch ( ex => {
            this.output("FAILURE", obj.reqMsg);
          });
        break;
      default:
        this.output("FAILURE", "Unknown message: " + obj.reqCode + "\n" + code)
    }
  }

  // ---------

  async quit () {
    if (this.windows.length   > 0) this.windows.pop();
    if (this.windows.length === 0) await this.browser.close();
  }
  async quitAll () { await this.browser.close(); }


  async open (url) {
    const win = this.activeWindow();
    await win.goto(url);
    // await page.screenshot({path: 'tst.png'});
  }

  async input (txt, n) {
    let win = this.activeWindow();

    const status = await win.evaluate(n => {
      var filterVisible = function (els) {
        var vis = [];
        for (var i = 0; i < els.length; i++)
          if (els[i].offsetParent !== null)
            vis.push(els[i]);
        return vis;
      };

      let els = document.querySelectorAll('input[type=search],input[type=text],input[type=password],textarea');
      let vis = filterVisible(els);
      if (vis.length <= n) return "Input not found";
      let el = vis[n];
      el.focus();
      el.setSelectionRange(0, el.value.length);
      return "OK";
    }, n);

    if (status !== "OK") {
      this.output("FAILURE", status);
      return 0;
    }

    await win.type(txt + String.fromCharCode(13));
    await win.waitForNavigation();
  }

  async query (q) {
    let win = this.activeWindow();
    let ret = await win.evaluate((q) => {
      searchResult = document.querySelectorAll(q);
      searchIndex  = 0;

      searchResult[searchIndex].tabIndex = 0;
      searchResult[searchIndex].focus();
      useFocus = document.activeElement == searchResult[searchIndex];
    }, q);
  }

  async search (q) {
    let win = this.activeWindow();
    let ret = await win.evaluate((q) => {
      let pat;
      let m = q.match(/(.+)\/(.+)/);

      if (m !== null && q[q.length-1] != '/') {
        pat = RegExp(m[2], m[3]);
      } else {
        pat = RegExp(q);
      }

      els = document.querySelectorAll('a,span,p');
      res = [];

      for (let i = 0; i < els.length; i++) {
        if (els[i].innerText.match(pat) !== null) {
          res.push(els[i]);
        }
      }

      for (let x = 0; x < res.length; x++) {
        let el = res[x];
        for (let i = res.length-1; i>=0; i--) {
          if (i==x) continue;
          if (!res[i].contains(el)) continue;
          res.splice(i, 1);
        }
      }

      searchResult = res;
      searchIndex  = 0;
      searchResult[searchIndex].tabIndex = 0;
      searchResult[searchIndex].focus();
      useFocus = document.activeElement == searchResult[searchIndex];
    }, q);
  }

  async yankText () {
    let win = this.activeWindow();
    let ret = await win.evaluate(() => {
      return (useFocus ? document.activeElement
                       : searchResult[searchIndex]
             ).innerText;
    });
    this.output("SUCCESS", ret.toString());
    return 0;
  }
  async yankURL () {
    let win = this.activeWindow();
    let ret = win.url();
    this.output("SUCCESS", ret.toString());
    return 0;
  }
  async yankAttribute (name) {
    let win = this.activeWindow();
    let ret = await win.evaluate(a => {
      let el = (useFocus ? document.activeElement
                         : searchResult[searchIndex]);

      if (el.getAttribute(a) !== null)
        return el.getAttribute(a)

      let pat;
      let m = a.match(/(.+)\/(.+)/);

      if (m !== null && a[a.length-1] != '/') {
        pat = RegExp(m[2], m[3]);
      } else {
        pat = RegExp(a);
      }

      let ats = el.getAttributeNames();
      res = [];

      for (let i = 0; i < ats.length; i++) {
        if (ats[i].match(pat) !== null) {
          res.push(ats[i]);
        }
      }

      return el.getAttribute(res[0]);
    }, name);

    this.output("SUCCESS", ret.toString());
    return 0;
  }

  async goUp () {
    let win = this.activeWindow();
    let rx  = /(.*((\/.+)*))(\/.+)/;
    let rs  = rx.exec(win.url());
    if (rs === null || rs[1] === null || rs[1] == "http:/" || rs[1] == "https:/")
      return;
    await win.goto(rs[1]);
  }
  async goRoot () {
    let win = this.activeWindow();
    let rx  = /(https?:\/\/)?[^\/]+/;
    let rs  = rx.exec(win.url());
    if (rs === null) return;
    await win.goto(rs[0]);
  }
  async goTop (n=0) {
    let win = this.activeWindow();
    let ret = await win.evaluate(n => {
      if (searchResult) {
        searchIndex = n;
        searchResult[searchIndex].tabIndex=0;
        searchResult[searchIndex].focus();
        useFocus = document.activeElement == searchResult[searchIndex];
      } else {
        window.scrollTo(0,0);
      }
    }, n);
  }
  async goBottom (n=0) {
    let win = this.activeWindow();
    let ret = await win.evaluate(n => {
      if (searchResult) {
        searchIndex = searchResult.length-1-n;
        searchResult[searchIndex].tabIndex=0;
        searchResult[searchIndex].focus();
        useFocus = document.activeElement == searchResult[searchIndex];
      } else {
        window.scrollTo(0, document.body.scrollHeight);
      }
    }, n);
  }


  async prev () {
    let win = this.activeWindow();
    let ret = await win.evaluate(() => {
      if (searchIndex == 0)
        return "No more results";

      searchIndex-=1;
      searchResult[searchIndex].tabIndex = 0;
      searchResult[searchIndex].focus();
      useFocus = document.activeElement == searchResult[searchIndex];
      return "OK"
    });

    if (ret !== "OK") {
      this.output("FAILURE", ret);
      return 0;
    }
  }
  async next () {
    let win = this.activeWindow();
    let ret = await win.evaluate(() => {
      if (searchIndex == searchResult.length-1)
        return "No more results";

      searchIndex+=1;
      searchResult[searchIndex].tabIndex = 0;
      searchResult[searchIndex].focus();
      useFocus = document.activeElement == searchResult[searchIndex];
      return "OK";
    });

    if (ret !== "OK") {
      this.output("FAILURE", ret);
      return 0;
    }
  }

  async click () {
    let win = this.activeWindow();
    let ret = await win.evaluate(() => {
      let el = (useFocus ? document.activeElement
                         : searchResult[searchIndex]);
      el.click();
      // let r = "";
      // for (let i = 0; i < 10; i++) {
      //   let q = el.tagName;
      //   if (el.id != "") q += "#" + el.id;
      //   if (el)
      //     for (let i = 0; i < el.classList.length; i++)
      //       q += "." + el.classList[i];
      //
      //   if (r != "") q += " > ";
      //   r = q + r;
      //
      //   el = el.parentElement;
      //   if (el == document.body || el === undefined) break;
      // }
      // return r;
    });
    // await win.click(ret);
    await win.waitForNavigation();
  }

  // Stolen from vimium
  async switchPage (pattern, rel) {
    await this.activeWindow().evaluate(args => {
      const followLink = el => {
        if (el.nodeName.toLowerCase() == "link") {
          window.location.href = el.href;
        } else {
          el.click();
        }
      }

      const findAndFollowRel = (value) => {
        let relTags = ["link", "a", "area"]
        for (let x = 0; x < relTags.length; x++) {
          let tag = relTags[x];
          let elements = document.getElementsByTagName(tag);
          for (let y = 0; y < elements.length; y++) {
            let element = elements[y];
            if (element.hasAttribute("rel") && element.rel.toLowerCase() == value) {
              followLink(element);
              return true;
            }
          }
        }
      };

      const makeXPath = (elementArray) => {
        let xpath = [];
        for (let i = 0; i < elementArray.length; i++) {
          let element = elementArray[i];
          xpath.push(".//" + element, ".//xhtml:" + element);
        }
        return xpath.join(" | ");
      };

      const evaluateXPath = (xpath, resultType) => {
        const contextNode = document.webkitIsFullScreen
                          ? document.webkitFullscreenElement
                          : document.documentElement;
        const namespaceResolver = (namespace) => {
          if (namespace === "xhtml") return "http://www.w3.org/1999/xhtml";
          return null;
        };
        return document.evaluate(xpath, contextNode, namespaceResolver, resultType, null);
      };

      const findAndFollowLink = (linkStrings) => {
        let link, linkString;
        const linksXPath = makeXPath(["a", "*[@onclick or @role='link' or contains(@class, 'button')]"]);
        links = evaluateXPath(linksXPath, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE);
        let candidateLinks = [];

        // at the end of this loop, candidateLinks will contain all visible links that match our patterns
        // links lower in the page are more likely to be the ones we want, so we loop through the snapshot backwards
        for (let i = links.snapshotLength - 1; i >= 0; i--) {
          link = links.snapshotItem(i);

          // ensure link is visible (we don't mind if it is scrolled offscreen)
          const boundingClientRect = link.getBoundingClientRect();
          if ((boundingClientRect.width === 0) || (boundingClientRect.height === 0)) {
            continue;
          }
          const computedStyle = window.getComputedStyle(link, null);
          if ((computedStyle.getPropertyValue("visibility") !== "visible") ||
              (computedStyle.getPropertyValue("display") === "none")) {
            continue;
          }

          let linkMatches = false;
          for (linkString of Array.from(linkStrings)) {
            if ((link.innerText.toLowerCase().indexOf(linkString) !== -1) ||
                (0 <= __guardMethod__(link.value, 'indexOf', o => o.indexOf(linkString)))) {
              linkMatches = true;
              break;
            }
          }
          if (!linkMatches) { continue; }

          candidateLinks.push(link);
        }

        if (candidateLinks.length === 0) { return; }

        for (link of Array.from(candidateLinks)) {
          link.wordCount = link.innerText.trim().split(/\s+/).length;
        }

        // We can use this trick to ensure that Array.sort is stable. We need this property to retain the reverse
        // in-page order of the links.

        candidateLinks.forEach((a,i) => a.originalIndex = i);

        // favor shorter links, and ignore those that are more than one word longer than the shortest link
        candidateLinks =
          candidateLinks
            .sort(function(a, b) {
              if (a.wordCount === b.wordCount) { return a.originalIndex - b.originalIndex; } else { return a.wordCount - b.wordCount; }
            })
            .filter(a => a.wordCount <= (candidateLinks[0].wordCount + 1));

        for (linkString of Array.from(linkStrings)) {
          const exactWordRegex =
            /\b/.test(linkString[0]) || /\b/.test(linkString[linkString.length - 1]) ?
              new RegExp(`\\b${linkString}\\b`, "i")
            :
              new RegExp(linkString, "i");
          for (let candidateLink of Array.from(candidateLinks)) {
            if (exactWordRegex.test(candidateLink.innerText) ||
                (candidateLink.value && exactWordRegex.test(candidateLink.value))) {
              followLink(candidateLink);
              return true;
            }
          }
        }
        return false;
      };
      function __guardMethod__(obj, methodName, transform) {
        if (typeof obj !== 'undefined' && obj !== null && typeof obj[methodName] === 'function') {
          return transform(obj, methodName);
        } else {
          return undefined;
        }
      }

      let pat = args[0];
      let rel = args[1];
      findAndFollowRel(rel) || findAndFollowLink(pat);
    }, [pattern, rel]);
  }

  async nextPage () {
    let win = this.activeWindow();
    const pat = "next,more,newer,>,›,→,»,≫,>>"
              . split(",").filter( s => s.trim().length);
    this.switchPage(pat, 'next');
    await win.waitForNavigation();
  }

  async prevPage () {
    let win = this.activeWindow();
    const pat = "prev,previous,back,older,<,‹,←,«,≪,<<"
              . split(",").filter( s => s.trim().length);
    this.switchPage(pat, 'prev');
    await win.waitForNavigation();
  }

  async nextOfType () {
    let win = this.activeWindow();
    let ret = await win.evaluate(() => {
      orig = searchResult[searchIndex];
      selection = orig;
      selector = "";
      for (let c = 0; c < 10; c++) {
        if (selection.classList.length <= 0) {
          selector = selection.tagName
          selection = selection.parentElement;
          continue;
        }
        let cur = ""
        for (let i = 0; i < selection.classList.length; i++) {
          cur += "." + selection.classList[i];
        }
        selector = cur + (selector == "" ? "" : " > " + selector);
        break;
      }
      searchResult = document.querySelectorAll(selector);
      for (searchIndex=0; searchIndex < searchResult.length; searchIndex++) {
        if ( searchResult[searchIndex] == orig
          || searchResult[searchIndex].contains(orig)) break;
      }
      if (searchIndex+1 >= searchResult.length) return selector;
      searchIndex+=1;
      searchResult[searchIndex].tabIndex=0;
      searchResult[searchIndex].focus();
      useFocus = document.activeElement == searchResult[searchIndex];
      return "OK";
    });

    if (ret !== "OK") {
      this.output("FAILURE", ret);
      return 0;
    }
  }

}

var halovi = new Halovi();

var rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
});

rl.on('line', function(ln){
  halovi.run(ln);
});

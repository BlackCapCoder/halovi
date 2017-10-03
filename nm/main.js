const readline  = require('readline');
const puppeteer = require('puppeteer');

class Halovi {
  constructor () {
    this.windows = [];
    (async () => {
      this.browser = await puppeteer.launch({args: ['--no-sandbox'], headless: true});
      let page = await this.browser.newPage();
      this.windows.push(page);
      console.log("ready");
    })();
  }

  run (code) { eval(code); }
  quit () {
    (async () => {
      if (this.windows.length > 0) this.windows.pop();
      if (this.windows.length == 0) await this.browser.close();
      console.log("OK");
    })();
  }
  quitAll () {
    (async () => {
      await this.browser.close();
      console.log("OK");
    })();
  }

  activeWindow () { return this.windows[this.windows.length-1]; }

  open (url) {
    let win = this.activeWindow();
    (async () => {
      await win.goto(url);
      // await page.screenshot({path: 'tst.png'});
      console.log("OK");
    })();
  }

  input (txt) {
    let win = this.activeWindow();

    (async () => {
      let status = await win.evaluate(() => {
        var filterVisible = function (els) {
          var vis = [];
          for (var i = 0; i < els.length; i++)
            if (els[i].offsetParent !== null)
              vis.push(els[i]);
          return vis;
        };

        let els = document.querySelectorAll('input[type=search],input[type=text],input[type=password],textarea');
        let vis = filterVisible(els);
        if (vis.length <= 0) return "Input not found";
        let el = vis[0];
        el.focus();
        el.setSelectionRange(0, el.value.length);
        return "OK";
      });

      await win.type(txt + String.fromCharCode(13));
      await win.waitForNavigation();
      console.log(status);
    })();
  }

  query (q) {
    let win = this.activeWindow();

    (async () => {
      let ret = await win.evaluate((q) => {
        searchResult = document.querySelectorAll(q);
        searchIndex  = 0;
      }, q);
      console.log("OK");
    })();
  }

  search (q) {
    let win = this.activeWindow();

    (async () => {
      let ret = await win.evaluate((q) => {
        pat = RegExp(q);
        els = document.querySelectorAll('a');
        res = [];
        for (let i = 0; i < els.length; i++)
            if (els[i].innerText.match(pat) !== null)
                res.push(els[i]);
        searchResult = res;
        searchIndex  = 0;
      }, q);
      console.log("OK");
    })();
  }

  yank () {
    let win = this.activeWindow();

    (async () => {
      let ret = await win.evaluate(() => {
        return searchResult[searchIndex].innerText;
      });
      console.log(ret);
    })();
  }

  next () {
    let win = this.activeWindow();

    (async () => {
      let ret = await win.evaluate(() => {
        if (searchIndex == searchResult.length-1)
          return "No more results";

        searchIndex++;
        return "OK"
      });
      console.log(ret);
    })();
  }

  click () {
    let win = this.activeWindow();

    (async () => {
      let ret = await win.evaluate(() => {
        searchResult[searchIndex].click();
      });
      await win.waitForNavigation();
      console.log("OK");
    })();
  }

  // Stolen from vimium
  switchPage (pattern, rel) {
    (async () => {
      return await this.activeWindow().evaluate(args => {
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
    })();
  }

  nextPage () {
    let win = this.activeWindow();
    (async () => {
      const pat = "next,more,newer,>,›,→,»,≫,>>"
                . split(",").filter( s => s.trim().length);
      this.switchPage(pat, 'next');
      await win.waitForNavigation();
      console.log("OK");
    })();
  }

  prevPage () {
    let win = this.activeWindow();
    (async () => {
      const pat = "prev,previous,back,older,<,‹,←,«,≪,<<"
                . split(",").filter( s => s.trim().length);
      this.switchPage(pat, 'prev');
      await win.waitForNavigation();
      console.log("OK");
    })();
  }

  nextOfType () {
    let win = this.activeWindow();
    (async () => {
      let ret = await win.evaluate(() => {
        orig = searchResult[searchIndex];
        selection = orig;
        selector = "";
        for (let c = 0; c < 10; c++) {
          if (selection.classList.length <= 0) {
            selection = selection.parentElement;
            continue;
          }
          for (let i = 0; i < selection.classList.length; i++) {
            selector += "." + selection.classList[i];
            break;
          }
        }
        searchResult = document.querySelectorAll(selector);
        for (searchIndex=0; searchIndex < searchResult.length; searchIndex++) {
          if (searchResult[searchIndex] == orig || searchResult[searchIndex].contains(orig)) break;
        }
        searchIndex++;
        if (searchIndex >= searchResult.length) return "No next element";
        return "OK";
      });
      console.log(ret);
    })();
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


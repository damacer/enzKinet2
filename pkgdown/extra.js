// EKA Documentation: small changes to pkgdown's pages.
document.addEventListener("DOMContentLoaded", function () {
  // Highlight the current page in the left side panel (see pkgdown/templates/before-body.html)
  var here = window.location.pathname.replace(/index\.html$/, "");
  document.querySelectorAll(".eka-sidebar a[href]").forEach(function (a) {
    var path = new URL(a.getAttribute("href"), window.location.href).pathname.replace(/index\.html$/, "");
    if (path === here) { a.classList.add("active"); a.setAttribute("aria-current", "page"); }
  });
  // Collapse the side panel on narrow screens, and open it again if the window widens
  var details = document.querySelector(".eka-sidebar-details");
  var narrow = window.matchMedia("(max-width: 991.98px)");
  function fitPanelToWidth() { if (details) details.open = !narrow.matches; }
  fitPanelToWidth();
  narrow.addEventListener("change", fitPanelToWidth);

  // Make each heading itself the link to its section, instead of a separate link icon
  document.querySelectorAll("main h2 > a.anchor, main h3 > a.anchor, main h4 > a.anchor").forEach(function (anchor) {
    var heading = anchor.parentElement;
    var link = document.createElement("a");
    link.className = "eka-heading-link";
    link.href = anchor.getAttribute("href");
    while (heading.firstChild && heading.firstChild !== anchor) { link.appendChild(heading.firstChild); }
    heading.insertBefore(link, anchor);
    anchor.remove();
  });

  // "Search" rather than "Search for"
  document.querySelectorAll("input[type=search], #search-input").forEach(function (input) {
    input.setAttribute("placeholder", "Search");
  });
  // Links that open in a new tab get rel="noopener"
  document.querySelectorAll("a[target=_blank]").forEach(function (a) { a.setAttribute("rel", "noopener"); });
});

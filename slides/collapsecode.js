//adapted from Emi Tanaka's gist at
//https://gist.github.com/emitanaka/eaa258bb8471c041797ff377704c8505
<script>
(function() {
  var divHTML = document.querySelectorAll(".hide-code");
  divHTML.forEach(function (el) {
    var preNodes = el.getElementsByTagName("pre");
    var outputNode = preNodes[0];
    outputNode.outerHTML = "<details class='r'><summary></summary>" + outputNode.outerHTML + "</details>";
  })
})();
</script>

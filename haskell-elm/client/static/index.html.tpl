<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Document</title>
  <link rel="stylesheet" href="css/bulma.css">
  <script src="https://use.fontawesome.com/e3f76b6a6c.js"></script>
</head>
<body>
  <div id="elm-root"></div>

  <script src="main.js"></script>
  <script>
    var options = { webSocketAddress: 'ws://<%= BIND %>:<%= PORT %>' };
    console.log('The server is bound on <%= BIND %>:<%= PORT %>');

    var node = document.getElementById('elm-root');
    var app = Elm.Main.embed(node, options);
  </script>
</body>
</html>

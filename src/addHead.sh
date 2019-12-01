#!/bin/bash
STDIN=$(cat -)
tail=${STDIN##*</head>}
cat site/docs/header.txt
echo $tail

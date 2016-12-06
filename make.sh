#!/bin/bash
exists()
{
	command -v "$1" >/dev/null 2>&1
}
if exists ghci; then
	ghci
else echo "haskell-platform should be installed"
	echo "Type sudo apt-get install haskell-platform"
fi
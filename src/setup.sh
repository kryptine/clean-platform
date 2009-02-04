#!/bin/sh
echo "Setting up the Clean Platform library collection"
echo "==="
echo "Creating 'Clean Platform.dirs'..."
find libraries -type d | grep -v "Clean System Files" | grep -v "\.svn" | grep -v "OS-[Windows|MacOS]" | xargs printf "$PWD/%s:" | xargs printf "%s." > "env/Clean Platform.dirs"
echo "==="
echo "You can now include the Clean platform libraries by adding \"-I \`cat \"env/Clean Platform.dirs\"\`\" to your clm options."

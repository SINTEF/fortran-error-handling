#!/bin/bash

# GitHub doesn't support embedding of text using the `include` statement when
# rendering the readme. As a workaround we generate a version where includes
# have been resolved using asciidoctor-reducer

readonly SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
asciidoctor-reducer $SCRIPT_DIR/README.adoc -o $SCRIPT_DIR/../README.adoc
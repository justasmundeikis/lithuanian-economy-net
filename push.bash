#!/bin/bash

bundle update && bundle exec jekyll build && git add . && git commit -m "update" && git push

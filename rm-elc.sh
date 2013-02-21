#!/bin/sh
find $1/packages -name '*.elc' | xargs rm
find $1/modules -name '*.elc' | xargs rm
find $1/profiles -name '*.elc' | xargs rm
find $1/smotitah -name '*.elc' | xargs rm

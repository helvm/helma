#!/bin/bash
alias etlas='../etlas'

etlas clean && etlas build && etlas test

git check-whitespace

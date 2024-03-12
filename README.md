# hocs

A simple command line tool to create a static documentation site from markdown files.

Created for an assignment in the course [Advanced functional programming (INF221)](https://www.uib.no/emne/INF221) at the University of Bergen.

## Installation

Clone the repo and run `stack install`. This will create an executable called `hocs-exe` in `bin/`.

You can use run `sudo make install` to install the executable to `/usr/local/bin`.

## Usage

### `hocs init`

Create a new documentation site in the current directory.

### `hocs build`

Build the documentation site from the markdown files in the current directory.

### `hocs serve`

Serve the documentation site on `localhost:3000`.

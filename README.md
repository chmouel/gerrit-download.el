## Introduction

`gerrit-download.el` will download a review from gerrit with the
git-review utility and show the diff of the review.

## Usage

Use M-x `gerrit-download` and it will ask for a project which should
be mapped locally on the filesystem from the variable
`magit-repo-dirs`. It will then ask for a review (be it a number or
change-id) and will use the `git-review` utility to download the
change and show the diff for that change.

## License

Apache

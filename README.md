# magit-vcsh
binding for vcsh in magit

Note that this is not well tested yet. Adding, commiting, pushing
and pulling should work, not sure for the rest.

Be aware of the problem of buffer naming: magit assume that there is
only one repository by directory, and vcsh break this assumption. So
if the buffer name is not "magit-vcsh something" there are risks.

# Use `vcsh-el` in Emacs

The cli tool uses environment variables to separate a git directory from the working tree. Which [is not suitable for interactive tools](https://github.com/magit/magit/issues/2939#issuecomment-271186636).

So it makes sense to replace `magit-vcsh` with `vcsh-el`, using `straight.el` because it's not published in Melpa yet. See https://github.com/magit/magit/issues/2939#issuecomment-519416818.

Maybe the cli tool enough though.

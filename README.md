# magit-vcsh
binding for vcsh in magit

Note that this is not well tested yet. Adding, commiting, pushing
and pulling should work, not sure for the rest.

Be aware of the problem of buffer naming: magit assume that there is
only one repository by directory, and vcsh break this assumption. So
if the buffer name is not "magit-vcsh something" there are risks.

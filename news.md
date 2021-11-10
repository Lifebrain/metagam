# metagam version 0.2.0.9001

- removed viridis from Suggests, to reduce the number of dependencies.
- removed gratia from Suggests.

# metagam version 0.2.0.9000

- Parallelism support removed, because it was buggy. Will refactor the code before considering adding it back.

# metagam version 0.2.0

- Proper support for parallelism added, thanks to Henrik Bengtsson. See example in help("metagam").

# metagam version 0.1.2.9002

- Fixed issue #30. Running in parallel caused error.

# metagam version 0.1.2.9001

- Updated citation after paper was published in NeuroImage.

# metagam version 0.1.2.9000

- Updated recommended citation after paper was accepted in NeuroImage.

# metagam version 0.1.2

- README is updated to suggest installation using Bioconductor
- A message is now printed to the user on startup if multtest is not installed. As a consequence, multtest has been added to Suggests:

# metagam version 0.1.1.9000

- Moved package 'viridis' from Imports to Suggests, since 'viridis' is hard to install on some platforms.

# metagam version 0.1.1

- Internal change which makes metagam compatible with dplyr 1.0.0.

# metagam version 0.1.0

- This is the initial release.

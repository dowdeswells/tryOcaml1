# Use the official OCaml/Opam image with OCaml 5.3.0 on Debian
FROM ocaml/opam:debian-ocaml-5.3 as opam


# Switch back to the 'opam' user for OCaml package management
USER opam

# Set the working directory inside the container
# For Dev Containers, this will be where your local project is mounted.
# The `ocaml/opam` images typically set `/home/opam` as the default WORKDIR.
# You can keep it, or set it to `/workspaces/${localWorkspaceFolderBasename}`
# which is common for Dev Containers. Let's stick with the devcontainer standard.
# WORKDIR /workspaces/${localWorkspaceFolderBasename}

# (Optional but recommended) Install common OCaml development tools
# These tools are generally useful for any OCaml project and enhance the VS Code experience.
RUN opam install ocaml-lsp-server ocamlformat dune utop

# You generally don't COPY your project files here when using Dev Containers,
# as VS Code mounts your local folder directly into the WORKDIR.
# However, if you were building a production image, you would COPY your code.
# COPY . .

# ENTRYPOINT and CMD are usually not set in Dev Container Dockerfiles
# as VS Code manages the container lifecycle and initial commands.
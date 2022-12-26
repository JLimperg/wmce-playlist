# wmce-playlist

This is a little script which generates a Spotify playlist from the monthly or
annual WMCE charts.

# Building

```text
$ cabal build
```

# Preparations

First, write the charts (from the email newsletter or the WMCE website) to a
text file, say `charts`. The file should look like this:

```text
1	THROISMA • Antonis Antoniou • Cyprus • Ajabu!
2	BABILONIA • Antonio Castrignano & Taranta Sounds • Italy • Ponderosa
3	ZAVRZLAMA • Divanhana • Bosnia • CPL Music
4	SPONDI • Stelios Petrakis Quartet • Greece • Technotropon Artway
...
```

Now create a new Spotify playlist and copy its ID. If the playlist's URL is

```text
https://open.spotify.com/playlist/abcdefg
```

then its ID is `abcdefg`.

Finally, obtain a Spotify OAuth2 access token. To do so, navigate to one of
the Spotify Console pages, e.g.

https://developer.spotify.com/console/post-playlist-tracks/

Press the button "Get Token" near the bottom of the page. Authorise the
creation of this token and select the scopes `playlist-modify-public` and
`playlist-modify-private`.

# Running `wmce-playlist`

Invoke the script like this:

```text
$ cabal run wmce-playlist <charts> <playlist>
```

`<charts>` is the charts text file; `<playlist>` is the playlist ID. If the
script succeeds in reading the charts, it asks for your OAuth2 access token.
After you've pasted the token, the script searches for the albums in the charts
and adds one song from each, chosen at random, to the playlist.

If the title and artists of an album do not match those in the charts file, the
script prints a warning. Often these mismatches are due to minor differences
in spelling, but sometimes they indicate that an album was not found and the
script used whatever other album Spotify's search function dug up.

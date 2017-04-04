app <- ShinyDriver$new("..")
app$snapshotInit("mytests")

app$snapshot()
app$snapshot()

app$snapshotCompare()



app: elm.js
	elm make SimpleTimeTracker.elm --output=elm.js

http:
	python -m SimpleHTTPServer 8080
#!/bin/bash

# Start the Gun relay server in the background
echo "Starting Gun relay server with Node.js..."
npm run gun-relay &
GUN_PID=$!

# Wait a moment for the Gun server to initialize
sleep 2

# Start the development server
echo "Starting development server..."
npm run dev

# When the development server is stopped, kill the Gun relay server
echo "Shutting down Gun relay server..."
kill $GUN_PID 
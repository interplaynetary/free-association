#!/bin/bash
# Start Gun and Holster relays separately
# Run this in a separate terminal if you need the relays

echo "Starting Gun and Holster relays..."
echo ""

# Start Gun relay
cd server/gun-relay
node server.js &
GUN_PID=$!
cd ../..

# Start Holster relay  
cd server/holster-relay
node server.js &
HOLSTER_PID=$!
cd ../..

echo ""
echo "✅ Relays started!"
echo "   Gun relay: http://localhost:8765/gun"
echo "   Holster relay: ws://localhost:8766/holster"
echo "   Holster health: http://localhost:8766/health"
echo ""
echo "To stop: kill $GUN_PID $HOLSTER_PID"
echo "Or press Ctrl+C"
echo ""

# Wait for both processes
wait


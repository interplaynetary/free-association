/**
 * Fast JSON Parser - Usage Examples and Benchmarks
 * 
 * Run with: bun run src/lib/utils/fastJsonParser.example.ts
 */

import {
	fastParse,
	fastParseSelective,
	fastParseWithExtraction,
	fastParseArrayFind,
	fastExtractTimestamp,
	fastValidateStructure,
	fastParseSafe,
	fastParseNetworkMessage
} from './fastJsonParser';

// Sample data
const largeCommitment = {
	id: 'user123',
	_updatedAt: 1704067200000,
	need_slots: Array.from({ length: 100 }, (_, i) => ({
		id: `slot-${i}`,
		name: `Need ${i}`,
		type: 'food',
		quantity: Math.random() * 100,
		metadata: {
			created: Date.now(),
			tags: ['tag1', 'tag2', 'tag3']
		}
	})),
	capacity_slots: Array.from({ length: 100 }, (_, i) => ({
		id: `cap-${i}`,
		name: `Capacity ${i}`,
		type: 'service',
		available: Math.random() * 100
	})),
	recognition_weights: Object.fromEntries(
		Array.from({ length: 50 }, (_, i) => [`user-${i}`, Math.random()])
	),
	metadata: {
		version: '5.0',
		lastSync: Date.now(),
		flags: ['active', 'verified']
	}
};

const jsonString = JSON.stringify(largeCommitment);
console.log(`JSON size: ${(jsonString.length / 1024).toFixed(2)} KB\n`);

// ═══════════════════════════════════════════════════════════════
// EXAMPLE 1: Timestamp Extraction (Most Common Use Case)
// ═══════════════════════════════════════════════════════════════

async function example1_TimestampExtraction() {
	console.log('═══ Example 1: Timestamp Extraction ═══');
	
	// Standard approach: Parse entire object
	console.time('Standard parse + timestamp');
	const data1 = JSON.parse(jsonString);
	const timestamp1 = data1._updatedAt;
	console.timeEnd('Standard parse + timestamp');
	console.log('Timestamp:', timestamp1);
	
	// Fast approach: Extract only timestamp
	console.time('Fast timestamp extraction');
	const timestamp2 = await fastExtractTimestamp(jsonString, '_updatedAt');
	console.timeEnd('Fast timestamp extraction');
	console.log('Timestamp:', timestamp2);
	
	console.log('Result:', timestamp1 === timestamp2 ? '✅ Same' : '❌ Different');
	console.log('');
}

// ═══════════════════════════════════════════════════════════════
// EXAMPLE 2: Selective Field Extraction
// ═══════════════════════════════════════════════════════════════

async function example2_SelectiveExtraction() {
	console.log('═══ Example 2: Selective Field Extraction ═══');
	
	const extracted = {
		id: null as any,
		timestamp: null as any,
		version: null as any,
		firstSlotName: null as any
	};
	
	console.time('Selective extraction');
	await fastParseSelective(jsonString, {
		'id': (value) => { extracted.id = value; },
		'_updatedAt': (value) => { extracted.timestamp = value; },
		'metadata.version': (value) => { extracted.version = value; },
		'need_slots[0].name': (value) => { extracted.firstSlotName = value; }
	});
	console.timeEnd('Selective extraction');
	
	console.log('Extracted:', extracted);
	console.log('');
}

// ═══════════════════════════════════════════════════════════════
// EXAMPLE 3: Array Search with Early Termination
// ═══════════════════════════════════════════════════════════════

async function example3_ArraySearch() {
	console.log('═══ Example 3: Array Search (Early Termination) ═══');
	
	const targetId = 'slot-25'; // In middle of 100-item array
	
	// Standard approach: Parse entire array
	console.time('Standard array search');
	const data = JSON.parse(jsonString);
	const found1 = data.need_slots.find((slot: any) => slot.id === targetId);
	console.timeEnd('Standard array search');
	console.log('Found:', found1?.name);
	
	// Fast approach: Stop at first match
	console.time('Fast array search');
	const found2 = await fastParseArrayFind(
		jsonString,
		'need_slots[*].id',
		(id) => id === targetId
	);
	console.timeEnd('Fast array search');
	console.log('Found ID:', found2);
	
	console.log('');
}

// ═══════════════════════════════════════════════════════════════
// EXAMPLE 4: Extraction + Full Parse
// ═══════════════════════════════════════════════════════════════

async function example4_ExtractionAndParse() {
	console.log('═══ Example 4: Extraction + Full Parse ═══');
	
	console.time('Extract metadata + full parse');
	const { extracted, full } = await fastParseWithExtraction(
		jsonString,
		['id', '_updatedAt', 'metadata.version']
	);
	console.timeEnd('Extract metadata + full parse');
	
	console.log('Extracted metadata:', extracted);
	console.log('Has full object:', !!full);
	console.log('Full object slots count:', full.need_slots?.length);
	console.log('');
}

// ═══════════════════════════════════════════════════════════════
// EXAMPLE 5: Structure Validation
// ═══════════════════════════════════════════════════════════════

async function example5_StructureValidation() {
	console.log('═══ Example 5: Structure Validation ═══');
	
	console.time('Validate structure');
	const validation = await fastValidateStructure(jsonString, {
		requiredFields: ['id', '_updatedAt', 'need_slots', 'capacity_slots'],
		optionalFields: ['metadata', 'recognition_weights']
	});
	console.timeEnd('Validate structure');
	
	console.log('Valid:', validation.valid ? '✅' : '❌');
	if (validation.missing) {
		console.log('Missing fields:', validation.missing);
	}
	console.log('');
}

// ═══════════════════════════════════════════════════════════════
// EXAMPLE 6: Network Message Parsing
// ═══════════════════════════════════════════════════════════════

async function example6_NetworkMessage() {
	console.log('═══ Example 6: Network Message Parsing ═══');
	
	const networkMsg = JSON.stringify({
		'#': 'track-id-12345',
		'@': 'reply-to-67890',
		put: {
			'user~pub.key': {
				'allocation/commitment': jsonString
			}
		}
	});
	
	console.time('Extract tracking ID');
	const { trackId, message } = await fastParseNetworkMessage(networkMsg);
	console.timeEnd('Extract tracking ID');
	
	console.log('Tracking ID:', trackId);
	console.log('Has message:', !!message);
	console.log('');
}

// ═══════════════════════════════════════════════════════════════
// EXAMPLE 7: Safe Parsing with Size Limit
// ═══════════════════════════════════════════════════════════════

async function example7_SafeParsing() {
	console.log('═══ Example 7: Safe Parsing with Size Limit ═══');
	
	// Test with normal size
	console.time('Safe parse (within limit)');
	const result1 = await fastParseSafe(jsonString, 1024 * 1024); // 1MB limit
	console.timeEnd('Safe parse (within limit)');
	console.log('Success:', result1.success ? '✅' : '❌');
	
	// Test with size limit exceeded
	console.time('Safe parse (exceed limit)');
	const result2 = await fastParseSafe(jsonString, 1024); // 1KB limit
	console.timeEnd('Safe parse (exceed limit)');
	console.log('Success:', result2.success ? '✅' : '❌');
	console.log('Error:', result2.error);
	console.log('');
}

// ═══════════════════════════════════════════════════════════════
// EXAMPLE 8: Real-World Store Update Scenario
// ═══════════════════════════════════════════════════════════════

async function example8_StoreUpdateScenario() {
	console.log('═══ Example 8: Real-World Store Update Scenario ═══');
	
	const lastSeen = 1704067100000; // 100 seconds before current
	
	// Simulate network update processing
	console.log('Scenario: Processing network update with timestamp check\n');
	
	// OLD APPROACH: Always parse everything
	console.log('Old approach:');
	console.time('  Parse + timestamp check + validation');
	const data = JSON.parse(jsonString);
	const timestamp = data._updatedAt;
	if (timestamp > lastSeen) {
		// Would validate and process here
		console.log('  → Data is new, would process');
	}
	console.timeEnd('  Parse + timestamp check + validation');
	
	// NEW APPROACH: Extract timestamp first
	console.log('\nNew approach:');
	console.time('  Extract timestamp');
	const fastTimestamp = await fastExtractTimestamp(jsonString, '_updatedAt');
	console.timeEnd('  Extract timestamp');
	
	if (fastTimestamp && fastTimestamp > lastSeen) {
		console.time('  Full parse + validation');
		const fastData = JSON.parse(jsonString);
		// Would validate here
		console.log('  → Data is new, processing');
		console.timeEnd('  Full parse + validation');
	} else {
		console.log('  → Data is stale, skipped full parse! ⚡');
	}
	
	console.log('');
}

// ═══════════════════════════════════════════════════════════════
// BENCHMARK: Comparing All Approaches
// ═══════════════════════════════════════════════════════════════

async function benchmark() {
	console.log('═══════════════════════════════════════════════════');
	console.log('PERFORMANCE BENCHMARK (10 iterations each)');
	console.log('═══════════════════════════════════════════════════\n');
	
	const iterations = 10;
	
	// Test 1: Full parse
	console.time('1. JSON.parse() x10');
	for (let i = 0; i < iterations; i++) {
		JSON.parse(jsonString);
	}
	console.timeEnd('1. JSON.parse() x10');
	
	// Test 2: Timestamp extraction
	console.time('2. fastExtractTimestamp() x10');
	for (let i = 0; i < iterations; i++) {
		await fastExtractTimestamp(jsonString, '_updatedAt');
	}
	console.timeEnd('2. fastExtractTimestamp() x10');
	
	// Test 3: Array find
	console.time('3. Array.find() after parse x10');
	for (let i = 0; i < iterations; i++) {
		const data = JSON.parse(jsonString);
		data.need_slots.find((s: any) => s.id === 'slot-50');
	}
	console.timeEnd('3. Array.find() after parse x10');
	
	console.time('4. fastParseArrayFind() x10');
	for (let i = 0; i < iterations; i++) {
		await fastParseArrayFind(jsonString, 'need_slots[*].id', (id) => id === 'slot-50');
	}
	console.timeEnd('4. fastParseArrayFind() x10');
	
	console.log('');
}

// ═══════════════════════════════════════════════════════════════
// Run All Examples
// ═══════════════════════════════════════════════════════════════

async function runAllExamples() {
	console.log('\n🚀 Fast JSON Parser - Examples & Benchmarks\n');
	console.log('═══════════════════════════════════════════════════\n');
	
	await example1_TimestampExtraction();
	await example2_SelectiveExtraction();
	await example3_ArraySearch();
	await example4_ExtractionAndParse();
	await example5_StructureValidation();
	await example6_NetworkMessage();
	await example7_SafeParsing();
	await example8_StoreUpdateScenario();
	await benchmark();
	
	console.log('═══════════════════════════════════════════════════');
	console.log('✅ All examples completed!');
	console.log('═══════════════════════════════════════════════════\n');
}

// Run if executed directly
if (import.meta.main) {
	runAllExamples().catch(console.error);
}

export { runAllExamples };


# NestedPie Component

A flexible layered pie chart component built with D3.js and Svelte.

## Features

- Display up to 6 distinct pie charts as concentric layers
- Intuitive API closely aligned with D3's conventions
- Fully customizable via props
- Interactive with hover effects and tooltips
- Event-driven architecture with custom events
- Elegant animations and transitions
- Responsive design

## Installation

This component requires D3.js as a dependency:

```bash
npm install d3
```

## Usage

```svelte
<script>
	import NestedPie from '../path/to/NestedPie.svelte';

	const layerData = [
		// Layer 0 (innermost)
		{
			name: 'Primary Categories',
			slices: [
				{ name: 'Category A', value: 30, color: '#3498db' },
				{ name: 'Category B', value: 25, color: '#e74c3c' },
				{ name: 'Category C', value: 20, color: '#2ecc71' }
			]
		},
		// Layer 1
		{
			name: 'Secondary Categories',
			slices: [
				{ name: 'Subcategory A1', value: 15, color: '#2980b9' },
				{ name: 'Subcategory A2', value: 15, color: '#2980b9' }
			]
		}
	];

	function handlePieClick(event) {
		console.log(event.detail);
	}
</script>

<NestedPie
	layers={layerData}
	width={600}
	height={600}
	centerRadius={60}
	ringThickness={40}
	padAngle={0.01}
	cornerRadius={3}
	on:pieClick={handlePieClick}
/>
```

## Props

| Prop            | Type                  | Default | Description                                         |
| --------------- | --------------------- | ------- | --------------------------------------------------- |
| `layers`        | `Array<PieChartData>` | `[]`    | Array of pie chart data objects to render as layers |
| `width`         | `number`              | `800`   | Width of the chart in pixels                        |
| `height`        | `number`              | `800`   | Height of the chart in pixels                       |
| `colorSchemes`  | `Array<Function>`     | `[]`    | Array of color functions for specific layers        |
| `margin`        | `number`              | `20`    | Margin around the chart                             |
| `maxLayers`     | `number`              | `6`     | Maximum number of layers to display                 |
| `centerRadius`  | `number`              | `50`    | Radius of the innermost pie chart                   |
| `ringThickness` | `number`              | `50`    | Thickness of each ring                              |
| `padAngle`      | `number`              | `0.01`  | Padding between pie segments                        |
| `cornerRadius`  | `number`              | `0`     | Corner radius for pie segments                      |
| `labels`        | `boolean`             | `true`  | Whether to show labels                              |

## Events

| Event      | Detail          | Description                         |
| ---------- | --------------- | ----------------------------------- |
| `pieClick` | `{data, layer}` | Fired when a pie segment is clicked |

## Data Structure

Each layer should follow this structure:

```typescript
interface PieSlice {
	name: string; // Name/label for the segment
	value: number; // Numeric value (determines segment size)
	color?: string; // Optional color for the segment
}

interface PieChartData {
	name: string; // Name of the layer
	slices: Array<PieSlice>; // Segments in this layer's pie chart
}
```

## Example

See `NestedPieExample.svelte` for a complete working example.

## Performance Considerations

For optimal performance:

1. Limit the number of segments per layer to improve readability
2. Consider disabling labels (`labels={false}`) for very small segments
3. Limit total data size for smoother animations
4. Adjust `ringThickness` based on the number of layers

## Customizing Appearance

You can customize the appearance of the chart by:

1. Providing custom `colorSchemes` functions for specific layers
2. Adjusting the `padAngle` and `cornerRadius` for different segment styles
3. Modifying the `centerRadius` and `ringThickness` to change proportions
4. Styling the container div with CSS

## Accessibility

For better accessibility:

1. The component provides tooltips on hover
2. Consider adding ARIA labels to your container
3. High contrast colors are recommended for better visibility

## Browser Compatibility

The component is compatible with all modern browsers that support SVG and D3.js.

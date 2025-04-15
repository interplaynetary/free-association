# ProtoMap Component for Svelte

This is a unified ProtoMap component for integrating Protomaps with MapLibre GL in Svelte applications.

## Features

- Load PMTiles directly from cloud storage without a tile server
- Support for vector, raster, and terrain (raster-dem) tiles
- Built-in theme support using protomaps-themes-base
- Easy theme switching with light, dark, white, black, and grayscale themes
- GeoJSON data overlay support
- Custom layer management
- Loading indicator
- TypeScript support

## Usage

### Basic Usage

```svelte
<script>
  import ProtoMap from './lib/ProtoMap.svelte';
  
  let mapComponent;
</script>

<ProtoMap
  bind:this={mapComponent}
  mapId="my-map"
  initialCenter={[0, 0]}
  initialZoom={2}
  pmtilesUrl="https://r2-public.protomaps.com/protomaps-sample-datasets/protomaps-basemap-opensource-20230408.pmtiles"
/>
```

### Styled Map with Theme

```svelte
<script>
  import ProtoMap from './lib/ProtoMap.svelte';
  
  let mapComponent;
  let theme = 'light';
  
  function toggleTheme() {
    theme = theme === 'light' ? 'dark' : 'light';
  }
</script>

<ProtoMap
  bind:this={mapComponent}
  mapId="my-styled-map"
  initialCenter={[0, 0]}
  initialZoom={2}
  pmtilesUrl="https://r2-public.protomaps.com/protomaps-sample-datasets/protomaps-basemap-opensource-20230408.pmtiles"
  useTheme={true}
  {theme}
/>

<button on:click={toggleTheme}>
  Switch to {theme === 'light' ? 'Dark' : 'Light'} Theme
</button>
```

### Adding Custom GeoJSON Data

```svelte
<script>
  import ProtoMap from './lib/ProtoMap.svelte';
  
  let mapComponent;
  
  function addMarker() {
    if (mapComponent) {
      // Add a GeoJSON source
      const sourceId = mapComponent.addGeoJSON('points', {
        type: 'FeatureCollection',
        features: [
          {
            type: 'Feature',
            geometry: {
              type: 'Point',
              coordinates: [0, 0]
            },
            properties: {
              title: 'Center Point'
            }
          }
        ]
      });
      
      // Add a layer for the source
      mapComponent.addLayer({
        id: 'marker-layer',
        type: 'circle',
        source: sourceId,
        paint: {
          'circle-radius': 8,
          'circle-color': '#FF0000'
        }
      });
    }
  }
</script>

<ProtoMap bind:this={mapComponent} />

<button on:click={addMarker}>Add Marker</button>
```

## Props

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| mapId | string | 'map' | ID for the map container |
| initialCenter | [number, number] | [0, 0] | Initial map center coordinates [lng, lat] |
| initialZoom | number | 1 | Initial zoom level |
| mapStyle | object | undefined | Optional MapLibre style specification |
| pmtilesUrl | string | undefined | URL to the PMTiles file |
| pmtilesSourceName | string | 'protomaps' | Name for the PMTiles source |
| pmtilesSourceType | 'vector' \| 'raster' \| 'raster-dem' | 'vector' | Type of PMTiles source |
| theme | 'light' \| 'dark' \| 'white' \| 'black' \| 'grayscale' | 'light' | Theme for styled maps |
| useTheme | boolean | false | Whether to use themed styling |

## Methods

| Method | Description |
|--------|-------------|
| flyTo(center, zoom) | Animate to a new center and zoom level |
| fitBounds(bounds, options) | Fit the map to a bounding box |
| getMap() | Get the MapLibre map instance |
| setTheme(theme) | Set the map theme |
| addLayer(layer) | Add a custom layer to the map |
| addGeoJSON(id, data) | Add a GeoJSON source to the map |

## Example

See the `ProtoMapExample.svelte` file for a complete example of how to use this component with different styles and themes.

## Dependencies

- maplibre-gl
- pmtiles
- protomaps-themes-base

## Installation

First, install the required dependencies:

```bash
bun add maplibre-gl pmtiles
bun add -d @types/maplibre-gl
```

Make sure to include the MapLibre GL CSS in your main HTML file or import it in your entry file:

```html
<link href="https://unpkg.com/maplibre-gl@3.6.2/dist/maplibre-gl.css" rel="stylesheet" />
```

## Components

This package includes two main components:

1. `ProtoMap.svelte` - A basic wrapper for MapLibre GL with PMTiles support
2. `StyledProtoMap.svelte` - A styled version that uses the Protomaps theme system for better cartography

### ProtoMap

This is the core component that provides direct access to MapLibre GL with PMTiles integration.

```svelte
<script>
  import ProtoMap from './lib/ProtoMap.svelte';
</script>

<ProtoMap
  mapId="my-map"
  initialCenter={[0, 0]}
  initialZoom={1}
  pmtilesUrl="https://r2-public.protomaps.com/protomaps-sample-datasets/protomaps-basemap-opensource-default.pmtiles"
  pmtilesSourceType="vector"
/>
```

#### Props

| Prop              | Type                                     | Default       | Description                                        |
|-------------------|------------------------------------------|---------------|----------------------------------------------------|
| mapId             | string                                   | 'map'         | HTML ID for the map container                      |
| initialCenter     | [number, number]                         | [0, 0]        | Initial center coordinates [lng, lat]              |
| initialZoom       | number                                   | 1             | Initial zoom level                                 |
| mapStyle          | maplibregl.StyleSpecification            | undefined     | Custom MapLibre GL style object                    |
| pmtilesUrl        | string                                   | undefined     | URL of the PMTiles archive                         |
| pmtilesSourceName | string                                   | 'protomaps'   | Name of the source in the map style               |
| pmtilesSourceType | 'vector' \| 'raster' \| 'raster-dem'     | 'vector'      | Type of PMTiles source                             |

### StyledProtoMap

This component builds on ProtoMap and adds theming capabilities from the Protomaps themes library. It provides pre-designed cartographic styles that look great out of the box.

```svelte
<script>
  import StyledProtoMap from './lib/StyledProtoMap.svelte';
</script>

<StyledProtoMap
  mapId="my-styled-map"
  initialCenter={[0, 0]}
  initialZoom={2}
  theme="light"
/>
```

#### Props

| Prop          | Type                                                   | Default       | Description                                |
|-----------------|--------------------------------------------------------|---------------|--------------------------------------------|
| mapId           | string                                                 | 'styled-map'  | HTML ID for the map container              |
| initialCenter   | [number, number]                                       | [0, 0]        | Initial center coordinates [lng, lat]      |
| initialZoom     | number                                                 | 2             | Initial zoom level                         |
| theme           | 'light' \| 'dark' \| 'white' \| 'black' \| 'grayscale' | 'light'       | Theme to use for styling the map           |
| pmtilesUrl      | string                                                 | *Protomaps basemap URL*  | URL of the PMTiles archive   |

## Methods

Both components expose the same methods for controlling the map:

### flyTo(center, zoom)

Animate the map to a new position.

```js
// Get reference to the component
let mapComponent;

// Fly to London
mapComponent.flyTo([-0.1278, 51.5074], 12);
```

### fitBounds(bounds, options)

Fit the map to a bounding box.

```js
// Fit to a bounding box
mapComponent.fitBounds([
  [-122.66, 37.78], // Southwest coordinates
  [-122.22, 37.95]  // Northeast coordinates
]);
```

### getMap()

Get the underlying MapLibre GL map instance for advanced operations.

```js
const mapInstance = mapComponent.getMap();
if (mapInstance) {
  // Do something with the native MapLibre map
  mapInstance.setPitch(45); // Set 3D perspective
}
```

## Examples

### Basic Vector Tiles

```svelte
<ProtoMap
  pmtilesUrl="https://r2-public.protomaps.com/protomaps-sample-datasets/protomaps-basemap-opensource-default.pmtiles"
  pmtilesSourceType="vector"
/>
```

### Building Data

```svelte
<ProtoMap
  pmtilesUrl="https://r2-public.protomaps.com/protomaps-sample-datasets/nz-buildings-v3.pmtiles"
  pmtilesSourceType="vector"
  initialCenter={[172.606201, -43.556510]}
  initialZoom={12}
/>
```

### Terrain

```svelte
<ProtoMap
  pmtilesUrl="https://r2-public.protomaps.com/protomaps-sample-datasets/terrarium_z9.pmtiles"
  pmtilesSourceType="raster-dem"
  initialCenter={[-117.177402, 48.994259]}
  initialZoom={10}
/>
```

### Styled Map

```svelte
<StyledProtoMap
  theme="dark"
  initialCenter={[0, 0]}
  initialZoom={2}
/>
```

## Advanced Usage

### Custom Styling

For more advanced usage with custom styling on the basic ProtoMap, you can pass a complete MapLibre GL style object:

```svelte
<script>
  // Define a custom style
  const customStyle = {
    version: 8,
    sources: {
      // Sources will be added by the component
    },
    layers: [
      // You can define custom layers here
      {
        id: 'custom-background',
        type: 'background',
        paint: {
          'background-color': '#f0f0f0'
        }
      }
    ]
  };
</script>

<ProtoMap
  mapStyle={customStyle}
  pmtilesUrl="https://r2-public.protomaps.com/protomaps-sample-datasets/protomaps-basemap-opensource-default.pmtiles"
  pmtilesSourceType="vector"
/>
```

## Notes on PMTiles

PMTiles is a single-file archive format for map tiles created by Protomaps. It enables direct loading of map tiles from cloud storage without requiring a dedicated tile server.

The `pmtiles` library provides a Protocol implementation that plugs into MapLibre GL to serve tiles directly from PMTiles archives.

For more information on PMTiles, visit [protomaps.com](https://protomaps.com).

## Attribution

When using PMTiles data, make sure to properly attribute the data source:

```svelte
<div class="attribution">
  Map data: © OpenStreetMap contributors | Tiles: Protomaps
</div>
``` 
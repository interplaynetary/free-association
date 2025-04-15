<script lang="ts">
  import { onMount } from 'svelte';
  import ProtoMap from './ProtoMap.svelte';
  
  // Store map component instance for method access
  let mapComponent: any;
  
  // Define interface for map styles
  interface MapStyle {
    id: string;
    name: string;
    url: string;
    type: 'vector' | 'raster' | 'raster-dem';
    center?: [number, number];
    zoom?: number;
  }

  // Define available map styles
  const mapStyles: MapStyle[] = [
    { 
      id: 'vector', 
      name: 'Vector', 
      url: 'https://r2-public.protomaps.com/protomaps-sample-datasets/protomaps-basemap-opensource-20230408.pmtiles', 
      type: 'vector',
      center: [0, 0],
      zoom: 2
    },
    {
      id: 'satellite',
      name: 'Satellite',
      url: 'https://api.maptiler.com/tiles/satellite/{z}/{x}/{y}.jpg?key=get_your_own_key',
      type: 'raster',
      center: [0, 0],
      zoom: 2
    },
    {
      id: 'terrain',
      name: 'Terrain',
      url: 'https://api.maptiler.com/tiles/terrain-rgb/{z}/{x}/{y}.png?key=get_your_own_key',
      type: 'raster-dem',
      center: [0, 0],
      zoom: 2
    }
  ];
  
  // Example mode - 'basic' or 'styled'
  let mode: 'basic' | 'styled' = 'basic';
  
  // Style switcher for basic map
  let currentStyle = 'vector';
  let styles: MapStyle[] = [
    { 
      id: 'vector', 
      name: 'Vector Basemap', 
      url: 'https://r2-public.protomaps.com/protomaps-sample-datasets/protomaps-basemap-opensource-20230408.pmtiles', 
      type: 'vector' 
    },
    { 
      id: 'buildings', 
      name: 'NZ Buildings', 
      url: 'https://protomaps-static.s3.amazonaws.com/osm_2021_nz_building_footprints.pmtiles', 
      type: 'vector', 
      center: [174.7645, -41.2865], 
      zoom: 12 
    },
    { 
      id: 'terrain', 
      name: 'Terrain', 
      url: 'https://protomaps-static.s3.amazonaws.com/cali_dlm_4326.pmtiles', 
      type: 'raster-dem', 
      center: [-117.177402, 38.994259], 
      zoom: 8 
    }
  ];
  
  // Theme switcher for styled map
  let theme: 'light' | 'dark' | 'white' | 'black' | 'grayscale' = 'light';
  let themes = [
    { id: 'light' as const, name: 'Light' },
    { id: 'dark' as const, name: 'Dark' },
    { id: 'white' as const, name: 'White' },
    { id: 'black' as const, name: 'Black' },
    { id: 'grayscale' as const, name: 'Grayscale' }
  ];
  
  let activeStyle = styles[0];
  let mapHeight = '550px';
  
  // Handle map click
  function handleMapClick(event: { detail: { lngLat: { lng: number; lat: number }; point: any; originalEvent: any } }) {
    const { lng, lat } = event.detail.lngLat;
    console.log(`Map clicked at: ${lng}, ${lat}`);
    
    // Add a marker at the clicked location
    mapComponent.addMarker([lng, lat], {
      color: '#FF0000',
      popup: `<h3>Marker at ${lng.toFixed(4)}, ${lat.toFixed(4)}</h3>
              <p>You clicked here!</p>`
    });
  }
  
  onMount(() => {
    // Add some initial markers
    setTimeout(() => {
      if (mapComponent) {
        // New York
        mapComponent.addMarker([-74.006, 40.7128], {
          color: '#FF5733',
          popup: '<h3>New York City</h3><p>The Big Apple</p>'
        });
        
        // London
        mapComponent.addMarker([-0.1278, 51.5074], {
          color: '#3366FF',
          popup: '<h3>London</h3><p>The capital of England</p>'
        });
        
        // Tokyo
        mapComponent.addMarker([139.6503, 35.6762], {
          color: '#33FF57',
          popup: '<h3>Tokyo</h3><p>The capital of Japan</p>'
        });
      }
    }, 2000);
  });
</script>

  <div class="map-container">
    <ProtoMap
      bind:this={mapComponent}
      mapId="protomap-example"
      initialCenter={mode === 'basic' ? (activeStyle.center || [0, 0]) : [0, 0]}
      initialZoom={mode === 'basic' ? (activeStyle.zoom || 1) : 2}
      pmtilesUrl={mode === 'basic' ? activeStyle.url : 'https://r2-public.protomaps.com/protomaps-sample-datasets/protomaps-basemap-opensource-20230408.pmtiles'}
      pmtilesSourceType="vector"
      theme={theme}
      height={mapHeight}
      showSearch={true}
      showControls={true}
      showStyleControls={true}
      on:mapclick={handleMapClick}
    />
  </div>

<style>
  .map-container {
    margin-bottom: 1rem;
  }
</style> 
import { createTreemap } from './visualizations/TreeMap.js';
import { createPieChart } from './visualizations/PieChart.js';
import { createPreziMap } from './visualizations/PreziMap.js';

export class App {
    constructor(data) {
        console.log('Constructor - Initial data:', data.name);
        this.data = data;
        this.visualizationType = 'treemap'; // Default visualization type
        this.init();
    }

    // New method to get a complete snapshot of the data state
    getDataSnapshot() {
        const snapshot = (node) => {
            const result = {
                name: node.name,
                points: node.points,
                types: node.types,
            };
            
            // Include children if they exist
            if (node.childrenArray) {
                result.children = node.childrenArray.map(child => snapshot(child));
            }
            
            return result;
        };
        
        return JSON.stringify(snapshot(this.data));
    }

    init() {
        const container = document.getElementById('treemap-container');
        const pieContainer = document.getElementById('pie-container');

        const width = container.clientWidth;
        const height = container.clientHeight;

        console.log('Init - Creating visualization with data:', this.data.name);
        
        // Create visualization based on current type
        if (this.visualizationType === 'treemap') {
            this.visualization = createTreemap(this.data, width, height);
        } else {
            this.visualization = createPreziMap(this.data, width, height);
        }
        
        console.log('Init - Visualization created');
        
        // Store initial data state after visualization is created
        this.lastDataState = this.getDataSnapshot();
        
        // Create initial pie chart
        this.updatePieChart();

        // Append visualizations
        container.appendChild(this.visualization.element);

        // Setup window resize handler
        window.addEventListener('resize', this.handleResize.bind(this));

        // Setup periodic checks for data changes
        setInterval(() => this.checkForDataChanges(), 100);
        
        // Add toggle button handler
        document.getElementById('toggle-viz').addEventListener('click', this.toggleVisualization.bind(this));
    }

    toggleVisualization() {
        this.visualizationType = this.visualizationType === 'treemap' ? 'prezimap' : 'treemap';
        this.updateVisualizations();
    }

    checkForDataChanges() {
        const currentDataState = this.getDataSnapshot();
        if (currentDataState !== this.lastDataState) {
            console.log('Data change detected, updating pie chart');
            this.lastDataState = currentDataState;
            this.updatePieChart();
        }
    }

    get currentView() {
        return this.visualization.getCurrentView();
    }

    get currentViewData() {
        return this.currentView.data;
    }

    get currentData() {
        return this.visualization.getCurrentData();
    }

    handleResize() {
        console.log('Resize handler triggered');
        const container = document.getElementById('treemap-container');
        const width = container.clientWidth;
        const height = container.clientHeight;

        this.visualization.update(width, height);
    }

    updateVisualizations() {
        console.log('UpdateVisualizations started');
        const container = document.getElementById('treemap-container');
        const width = container.clientWidth;
        const height = container.clientHeight;
        
        // Store the current data path to restore zoom if possible
        const currentData = this.currentViewData;
        
        // Clear container
        container.innerHTML = '';
        
        // Create new visualization based on current type
        if (this.visualizationType === 'treemap') {
            this.visualization = createTreemap(this.data, width, height);
        } else {
            this.visualization = createPreziMap(this.data, width, height);
        }
        
        container.appendChild(this.visualization.element);

        // Update last data state after visualization update
        this.lastDataState = this.getDataSnapshot();
        
        // Update button text
        document.getElementById('toggle-viz').textContent = 
            this.visualizationType === 'treemap' ? 'Switch to PreziMap' : 'Switch to TreeMap';
    }

    updatePieChart() {
        const pieContainer = document.getElementById('pie-container');
        const currentData = this.currentData;
        pieContainer.innerHTML = '';
        const newPieChart = createPieChart(currentData);
        pieContainer.appendChild(newPieChart);
    }
}
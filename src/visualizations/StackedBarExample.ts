import { createStackedBar, createMultiStackedBar } from './StackedBar';
import { TreeNode } from '../models/TreeNode';

/**
 * Example of how to use the stacked bar chart components
 * 
 * @param rootNode - The root TreeNode containing the data
 */
export function renderStackedBarExamples(rootNode: TreeNode) {
    // Basic usage - single stacked bar, horizontal orientation
    const singleContainer = document.getElementById('single-bar-container');
    if (singleContainer) {
        const barChart = createStackedBar(rootNode, 'single-bar-container', 'horizontal');
        singleContainer.appendChild(barChart!);
    }

    // Vertical orientation example
    const verticalContainer = document.getElementById('vertical-bar-container');
    if (verticalContainer) {
        const verticalBarChart = createStackedBar(rootNode, 'vertical-bar-container', 'vertical');
        verticalContainer.appendChild(verticalBarChart!);
    }

    // Example of multiple bars for comparison
    // First we need multiple nodes - in a real scenario these would be different nodes
    // For this example, we'll use the same root node and some of its children
    const childNodes = Array.from(rootNode.children.values());
    
    // Only proceed if we have at least 2 children to compare
    if (childNodes.length >= 2) {
        const nodesToCompare = [rootNode, ...childNodes.slice(0, 2)]; // Root + first 2 children
        const labels = ['Root Node', 'Child 1', 'Child 2'];
        
        const multiContainer = document.getElementById('multi-bar-container');
        if (multiContainer) {
            const multiBarChart = createMultiStackedBar(
                nodesToCompare,
                'multi-bar-container',
                'horizontal',
                labels
            );
            multiContainer.appendChild(multiBarChart!);
        }
        
        // Vertical multi-bar example
        const verticalMultiContainer = document.getElementById('vertical-multi-bar-container');
        if (verticalMultiContainer) {
            const verticalMultiBarChart = createMultiStackedBar(
                nodesToCompare,
                'vertical-multi-bar-container',
                'vertical',
                labels
            );
            verticalMultiContainer.appendChild(verticalMultiBarChart!);
        }
    }
}

/**
 * Helper function to create container divs for the examples
 */
export function setupStackedBarContainers() {
    // Create container elements if they don't exist
    const containers = [
        { id: 'single-bar-container', title: 'Single Stacked Bar (Horizontal)' },
        { id: 'vertical-bar-container', title: 'Single Stacked Bar (Vertical)' },
        { id: 'multi-bar-container', title: 'Multiple Stacked Bars (Horizontal)' },
        { id: 'vertical-multi-bar-container', title: 'Multiple Stacked Bars (Vertical)' }
    ];
    
    const mainContainer = document.getElementById('stacked-bar-examples') 
        || document.createElement('div');
    
    if (!mainContainer.id) {
        mainContainer.id = 'stacked-bar-examples';
        document.body.appendChild(mainContainer);
    }
    
    containers.forEach(container => {
        if (!document.getElementById(container.id)) {
            const containerDiv = document.createElement('div');
            containerDiv.id = container.id;
            containerDiv.style.width = '800px';
            containerDiv.style.height = '400px';
            containerDiv.style.margin = '20px';
            containerDiv.style.padding = '10px';
            containerDiv.style.border = '1px solid #ccc';
            
            const titleElement = document.createElement('h3');
            titleElement.textContent = container.title;
            containerDiv.appendChild(titleElement);
            
            mainContainer.appendChild(containerDiv);
        }
    });
}

/**
 * Main function to render all stacked bar examples
 * 
 * @param rootNode - The root TreeNode containing the data
 */
export function renderStackedBarVisualizations(rootNode: TreeNode) {
    setupStackedBarContainers();
    renderStackedBarExamples(rootNode);
} 
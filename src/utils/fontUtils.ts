import { type RecNode } from "../stores/rec"; 

/**
 * Calculate the appropriate font size for a node's text
 * @param {Object} d - The node data
 * @param {number} rectWidth - The width of the rectangle
 * @param {number} rectHeight - The height of the rectangle
 * @param {Object} root - The root node
 * @param {Object} x - The x scale
 * @param {Object} y - The y scale
 * @param {Object} currentView - The current view node
 */
export function calculateFontSize(d: d3.HierarchyRectangularNode<RecNode>, root: d3.HierarchyRectangularNode<RecNode>, x: d3.ScaleLinear<number, number>, y: d3.ScaleLinear<number, number>, currentView: d3.HierarchyRectangularNode<RecNode>) {
    // Always use the current view's domain for calculations
    const currentDomain = currentView || root;
    
    // Calculate actual dimensions based on current view
    const actualWidth = Math.abs(x(d.x1) - x(d.x0));
    const actualHeight = Math.abs(y(d.y1) - y(d.y0));
    
    const textContent = d === root ? name(d) : d.data.name;
    const lines = d === root ? [textContent] : textContent?.split(/(?=[A-Z][^A-Z])/g) || [];
    const maxLineLength = Math.max(...lines.map(line => line?.length || 0));
    
    // Use consistent dimensions for calculations
    const availableWidth = actualWidth * 0.9;
    const availableHeight = (actualHeight * 0.9) / (lines.length * 1.2);
    
    const widthBasedSize = availableWidth / (maxLineLength * 0.6);
    const heightBasedSize = availableHeight * 0.8;
    
    const size = Math.min(widthBasedSize, heightBasedSize);
    
    /*
    console.log(`Font size calculation for ${d.data.name}:`, {
        view: currentView ? currentView.data.name : 'root',
        dimensions: { actualWidth, actualHeight },
        result: Math.min(Math.max(size, 8), 24)
    });*/
    
    return Math.min(Math.max(size, 8), 24);
}

// Helper function to get the full name path
export function name(d: RecNode) {
    const names = [];
    let current = d;
    while (current) {
        names.push(current.data.name);
        current = current.parent;
    }
    return names.reverse().join(" / ");
};
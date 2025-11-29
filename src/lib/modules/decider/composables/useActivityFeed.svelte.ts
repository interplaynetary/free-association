/**
 * @module useActivityFeed
 * Track user actions with timestamps and enable undo
 * Elegant activity tracking for confidence and transparency
 */

export interface Activity {
	id: string;
	type: 'proposal' | 'challenge' | 'comment' | 'modification' | 'support';
	timestamp: Date;
	targetPub?: string;
	content?: string;
	data?: any;
}

export function useActivityFeed() {
	let activities = $state<Activity[]>([]);
	let maxActivities = 50; // Keep last 50 actions
	
	function add(activity: Omit<Activity, 'id' | 'timestamp'>) {
		const newActivity: Activity = {
			...activity,
			id: `${Date.now()}-${Math.random().toString(36).slice(2)}`,
			timestamp: new Date()
		};
		
		activities = [newActivity, ...activities].slice(0, maxActivities);
	}
	
	function remove(id: string) {
		activities = activities.filter(a => a.id !== id);
	}
	
	function clear() {
		activities = [];
	}
	
	function getRecent(count: number = 5): Activity[] {
		return activities.slice(0, count);
	}
	
	function getByType(type: Activity['type']): Activity[] {
		return activities.filter(a => a.type === type);
	}
	
	return {
		get activities() { return activities; },
		get recentCount() { return activities.length; },
		add,
		remove,
		clear,
		getRecent,
		getByType
	};
}




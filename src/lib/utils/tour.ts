import { driver } from 'driver.js';

export function startTour() {
	const driverObj = driver({
		showProgress: true,
		nextBtnText: 'Next →',
		prevBtnText: '← Back',
		doneBtnText: 'Start Creating!',
		steps: [
			{
				popover: {
					title: 'Welcome to Free Association! 🌱',
					description:
						"A way to recognize contributions and create abundance together. Let's see how it works.",
					side: 'bottom',
					align: 'center'
				}
			},
			{
				element: '.breadcrumbs',
				popover: {
					title: 'Your Recognition Tree',
					description:
						'This shows where you are in your tree. You start at yourself - the root of all your recognition.',
					side: 'bottom',
					align: 'start'
				}
			},
			{
				popover: {
					title: 'Recognition Works Like This',
					description:
						'You have your total recognition to give. You decide how much goes to each person who contributes to your life.',
					side: 'bottom',
					align: 'center'
				}
			},
			{
				element: '.add-button',
				popover: {
					title: 'Add Contributors',
					description:
						'Add people, projects, or things that contribute to your wellbeing. Each gets a portion of your recognition.',
					side: 'bottom',
					align: 'center'
				}
			},
			{
				element: '.parent',
				popover: {
					title: 'Your Canvas',
					description:
						'Here you build your recognition tree. Each node is a contributor. Set how much recognition each deserves.',
					side: 'top',
					align: 'center'
				}
			},
			{
				popover: {
					title: 'Mutual Recognition',
					description:
						'When you recognize someone AND they recognize you, that creates mutual recognition. It takes both sides.',
					side: 'bottom',
					align: 'center'
				}
			},
			{
				popover: {
					title: 'The Balance',
					description:
						'If you value them highly but they barely value you, your mutual recognition stays small. Both sides matter.',
					side: 'bottom',
					align: 'center'
				}
			},
			{
				element: '.bars .bar-group:first-child',
				popover: {
					title: 'Your Recognition',
					description:
						'This shows how you split your recognition among contributors. Each slice represents someone you value.',
					side: 'left',
					align: 'center'
				}
			},
			{
				element: '.bars .bar-group:last-child',
				popover: {
					title: 'Mutual Recognition',
					description:
						'This shows your mutual recognition with each person. This determines their share in your capacities.',
					side: 'left',
					align: 'center'
				}
			},
			{
				popover: {
					title: 'How Sharing Works',
					description:
						'People share their capacities with those they mutually recognize. The stronger the mutual recognition, the larger the share.',
					side: 'bottom',
					align: 'center'
				}
			},
			{
				popover: {
					title: 'Why Honesty Works',
					description:
						'If you give false recognition, you miss out on real connections. Honest recognition leads to genuine abundance.',
					side: 'bottom',
					align: 'center'
				}
			},
			{
				element: '.search-button',
				popover: {
					title: 'Navigate & Search',
					description:
						'Find anyone in your growing tree of recognition. As networks grow, navigation helps you stay connected.',
					side: 'bottom',
					align: 'center'
				}
			},
			{
				popover: {
					title: 'Ready to Begin',
					description:
						'Free Association grows through honest recognition. Start by adding someone who truly contributes to your life!',
					side: 'bottom',
					align: 'center'
				}
			}
		]
	});

	driverObj.drive();
}

import React, { useState, useEffect } from 'react';
import { Box, Text } from 'ink';
import { Logo } from './Logo';
import { Menu, type MenuItem } from './Menu';
import { Dashboard } from './screens/Dashboard';
import { CapacityList } from './screens/CapacityList';
import { CapacityAddV3 as CapacityAdd } from './screens/CapacityAddV3';
import { CapacityDetail } from './screens/CapacityDetail';
import { NeedList } from './screens/NeedList';
import { NeedAddV3 as NeedAdd } from './screens/NeedAddV3';
import { NeedDetail } from './screens/NeedDetail';
import { FindProviders } from './screens/FindProviders';
import { FindRecipients } from './screens/FindRecipients';
import { AllocationResults } from './screens/AllocationResults';
import { StepByStepDisplay } from './StepByStepDisplay';
import * as storage from './data/storage';
import type { AvailabilitySlot, NeedSlot } from '../schemas';

type Screen =
	| 'logo'
	| 'menu'
	| 'dashboard'
	| 'capacities'
	| 'capacity-add'
	| 'capacity-edit'
	| 'capacity-detail'
	| 'needs'
	| 'need-add'
	| 'need-edit'
	| 'find-providers'
	| 'find-recipients'
	| 'allocations'
	| 'stepByStep'
	| 'exit';

export const CLIApp: React.FC = () => {
	const [screen, setScreen] = useState<Screen>('logo');
	const [selectedCapacityId, setSelectedCapacityId] = useState<string | null>(null);
	const [selectedNeedId, setSelectedNeedId] = useState<string | null>(null);

	// Load config
	const [config, setConfig] = useState(storage.loadConfig());
	
	// If no config, generate a default one
	useEffect(() => {
		if (!config) {
			const newConfig: storage.CLIConfig = {
				myPubKey: `user-${Date.now()}`,
				myName: 'User'
			};
			storage.saveConfig(newConfig);
			setConfig(newConfig);
		}
	}, [config]);

	// Load data
	const [capacities, setCapacities] = useState<AvailabilitySlot[]>([]);
	const [needs, setNeeds] = useState<NeedSlot[]>([]);

	const refreshData = () => {
		if (config) {
			setCapacities(storage.listMyCapacities(config.myPubKey));
			setNeeds(storage.listMyNeeds(config.myPubKey));
		}
	};

	useEffect(() => {
		refreshData();
	}, [config]);

	// Auto-transition from logo to menu
	useEffect(() => {
		if (screen === 'logo') {
			const timer = setTimeout(() => setScreen('menu'), 2000);
			return () => clearTimeout(timer);
		}
	}, [screen]);

	const menuItems: MenuItem[] = [
		{
			id: 'dashboard',
			label: '📊 Dashboard',
			description: 'View your overview'
		},
		{
			id: 'capacities',
			label: '📦 Manage Capacities',
			description: 'What you can offer'
		},
		{
			id: 'needs',
			label: '🎯 Manage Needs',
			description: 'What you need'
		},
		{
			id: 'find-providers',
			label: '🔍 Find Providers',
			description: 'Who can help you?'
		},
		{
			id: 'find-recipients',
			label: '🎁 Find Recipients',
			description: 'Who needs your help?'
		},
		{
			id: 'allocations',
			label: '📊 View Allocations',
			description: 'See the results'
		},
		{
			id: 'stepByStep',
			label: '▶️  Step-by-Step Allocation',
			description: 'Watch convergence in action'
		},
		{
			id: 'exit',
			label: '🚪 Exit',
			description: 'Quit the application'
		}
	];

	const handleMenuSelect = (itemId: string) => {
		setScreen(itemId as Screen);
	};

	const handleCapacityAdd = (capacity: AvailabilitySlot) => {
		if (config) {
			storage.addCapacity(config.myPubKey, capacity);
			refreshData();
			setScreen('capacities');
		}
	};

	const handleCapacityDelete = (capacityId: string) => {
		if (config) {
			storage.removeCapacity(config.myPubKey, capacityId);
			refreshData();
			setScreen('capacities');
		}
	};

	const handleNeedAdd = (need: NeedSlot) => {
		if (config) {
			storage.addNeed(config.myPubKey, need);
			refreshData();
			setScreen('needs');
		}
	};

	const handleNeedDelete = (needId: string) => {
		if (config) {
			storage.removeNeed(config.myPubKey, needId);
			refreshData();
			setScreen('needs');
		}
	};

	if (!config) {
		return (
			<Box padding={2}>
				<Text>Initializing...</Text>
			</Box>
		);
	}

	return (
		<Box flexDirection="column">
			{screen === 'logo' && <Logo />}

			{screen === 'menu' && (
				<Box flexDirection="column">
					<Logo subtitle="Free Association Protocol v5" />
					<Menu
						items={menuItems}
						onSelect={handleMenuSelect}
						onExit={() => setScreen('exit')}
					/>
				</Box>
			)}

			{screen === 'dashboard' && (
				<Dashboard
					myName={config.myName}
					capacities={capacities}
					needs={needs}
					onNavigate={(dest) => {
						if (dest === 'capacities') setScreen('capacities');
						else if (dest === 'needs') setScreen('needs');
						else if (dest === 'allocate') setScreen('stepByStep');
						else setScreen('menu');
					}}
				/>
			)}

			{screen === 'capacities' && (
				<CapacityList
					capacities={capacities}
					onSelect={(id) => {
						setSelectedCapacityId(id);
						setScreen('capacity-detail');
					}}
					onAdd={() => setScreen('capacity-add')}
					onBack={() => setScreen('menu')}
				/>
			)}

			{screen === 'capacity-add' && (
				<CapacityAdd
					onSave={handleCapacityAdd}
					onCancel={() => setScreen('capacities')}
				/>
			)}

		{screen === 'capacity-detail' && selectedCapacityId && (
			<CapacityDetail
				capacity={storage.getCapacity(config.myPubKey, selectedCapacityId)!}
				onBack={() => setScreen('capacities')}
				onEdit={() => {
					setScreen('capacity-edit');
				}}
				onDelete={() => {
					handleCapacityDelete(selectedCapacityId);
				}}
			/>
		)}

		{screen === 'capacity-edit' && selectedCapacityId && (
			<CapacityAdd
				existingCapacity={storage.getCapacity(config.myPubKey, selectedCapacityId)!}
				onSave={(capacity) => {
					if (config) {
						storage.updateCapacity(config.myPubKey, selectedCapacityId, capacity);
						refreshData();
						setScreen('capacity-detail');
					}
				}}
				onCancel={() => setScreen('capacity-detail')}
			/>
		)}

		{screen === 'needs' && (
			<NeedList
				needs={needs}
				onSelect={(id) => {
					setSelectedNeedId(id);
					setScreen('need-detail');
				}}
				onAdd={() => setScreen('need-add')}
				onBack={() => setScreen('menu')}
			/>
		)}

		{screen === 'need-add' && (
			<NeedAdd
				onSave={handleNeedAdd}
				onCancel={() => setScreen('needs')}
			/>
		)}

		{screen === 'need-detail' && selectedNeedId && (
			<NeedDetail
				need={storage.getNeed(config.myPubKey, selectedNeedId)!}
				onBack={() => setScreen('needs')}
				onEdit={() => {
					setScreen('need-edit');
				}}
				onDelete={() => {
					handleNeedDelete(selectedNeedId);
				}}
			/>
		)}

		{screen === 'need-edit' && selectedNeedId && (
			<NeedAdd
				existingNeed={storage.getNeed(config.myPubKey, selectedNeedId)!}
				onSave={(need) => {
					if (config) {
						storage.updateNeed(config.myPubKey, selectedNeedId, need);
						refreshData();
						setScreen('need-detail');
					}
				}}
				onCancel={() => setScreen('need-detail')}
			/>
		)}

		{screen === 'find-providers' && (
			<FindProviders
				myPubKey={config?.myPubKey || ''}
				myNeeds={needs}
				allCommitments={storage.loadCommitments() || {}}
				onBack={() => setScreen('menu')}
				onSelectNeed={(id) => {
					// Could navigate to need detail if implemented
					setScreen('needs');
				}}
			/>
		)}

		{screen === 'find-recipients' && (
			<FindRecipients
				myPubKey={config?.myPubKey || ''}
				myCapacities={capacities}
				allCommitments={storage.loadCommitments() || {}}
				onBack={() => setScreen('menu')}
				onSelectCapacity={(id) => {
					// Could navigate to capacity detail if implemented
					setScreen('capacities');
				}}
			/>
		)}

	{screen === 'allocations' && (
		<AllocationResults
			myPubKey={config?.myPubKey || ''}
			allCommitments={storage.loadCommitments() || {}}
			onBack={() => setScreen('menu')}
		/>
	)}

	{screen === 'stepByStep' && (
		<StepByStepDisplay onBack={() => setScreen('menu')} />
	)}

	{screen === 'exit' && (
		<Box borderStyle="round" borderColor="magenta" padding={2}>
			<Text color="magenta">👋 Goodbye! Thanks for playing!</Text>
		</Box>
	)}
		</Box>
	);
};


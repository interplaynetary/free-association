import React from 'react';
import { Box, Text } from 'ink';

export interface TableColumn {
	key: string;
	label: string;
	width?: number;
	align?: 'left' | 'right' | 'center';
	render?: (value: any, row: any) => React.ReactNode;
}

interface TableProps {
	columns: TableColumn[];
	data: any[];
	emptyMessage?: string;
}

export const Table: React.FC<TableProps> = ({
	columns,
	data,
	emptyMessage = 'No data'
}) => {
	if (data.length === 0) {
		return (
			<Box>
				<Text dimColor>{emptyMessage}</Text>
			</Box>
		);
	}

	const renderCell = (column: TableColumn, value: any, row: any, width: number) => {
		const content = column.render ? column.render(value, row) : String(value || '');
		const contentStr = typeof content === 'string' ? content : '';
		const align = column.align || 'left';
		
		if (align === 'right') {
			return contentStr.padStart(width);
		} else if (align === 'center') {
			const padding = Math.max(0, width - contentStr.length);
			const leftPad = Math.floor(padding / 2);
			return ' '.repeat(leftPad) + contentStr;
		}
		return contentStr;
	};

	return (
		<Box flexDirection="column">
			{/* Header */}
			<Box>
				{columns.map((col) => (
					<Box key={col.key} width={col.width} marginRight={2}>
						<Text bold>{col.label}</Text>
					</Box>
				))}
			</Box>
			
			{/* Separator */}
			<Box>
				{columns.map((col) => (
					<Box key={col.key} width={col.width} marginRight={2}>
						<Text dimColor>{'─'.repeat(col.width || col.label.length)}</Text>
					</Box>
				))}
			</Box>
			
			{/* Rows */}
			{data.map((row, index) => (
				<Box key={index}>
					{columns.map((col) => (
						<Box key={col.key} width={col.width} marginRight={2}>
							{typeof col.render === 'function' ? (
								col.render(row[col.key], row)
							) : (
								<Text>{renderCell(col, row[col.key], row, col.width || col.label.length)}</Text>
							)}
						</Box>
					))}
				</Box>
			))}
		</Box>
	);
};


import { json } from '@sveltejs/kit';
import type { RequestHandler } from './$types';
import { prisma } from '$lib/server/prisma';
import bcrypt from 'bcryptjs';
import crypto from 'crypto';

export const POST: RequestHandler = async (event) => {
	const { request } = event;
	const { username, password } = await request.json();

	// Input validation
	if (!username || !password) {
		return json(
			{
				success: false,
				message: 'Username and password are required'
			},
			{ status: 400 }
		);
	}

	if (password.length < 6) {
		return json(
			{
				success: false,
				message: 'Password must be at least 6 characters long'
			},
			{ status: 400 }
		);
	}

	try {
		// Check if username already exists
		const existingUser = await prisma.user.findFirst({
			where: { name: username }
		});

		if (existingUser) {
			return json(
				{
					success: false,
					message: 'Username already exists'
				},
				{ status: 409 }
			);
		}

		// Hash password
		const hashedPassword = await bcrypt.hash(password, 10);

		// Generate unique ID
		const userId = crypto.randomUUID();

		// Create new user with raw query
		await prisma.$executeRaw`
			INSERT INTO User (id, name, email, password, role)
			VALUES (${userId}, ${username}, ${`${username}@example.com`}, ${hashedPassword}, 'user')
		`;

		// Fetch the created user
		const user = await prisma.user.findUnique({
			where: { id: userId }
		});

		return json({
			success: true,
			user: {
				id: user?.id,
				username: user?.name
			}
		});
	} catch (error) {
		console.error('Registration error:', error);
		return json(
			{
				success: false,
				message: 'An error occurred during registration'
			},
			{ status: 500 }
		);
	}
};

import { json } from '@sveltejs/kit';
import type { RequestHandler } from './$types';
import { prisma } from '$lib/server/prisma';
import bcrypt from 'bcryptjs';

// Extended User type with password
type UserWithPassword = {
	id: string;
	name: string | null;
	email: string | null;
	image: string | null;
	password: string | null;
	role: string | null;
};

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

	try {
		// Find user in database
		const user = await prisma.user.findFirst({
			where: { name: username }
		});

		if (!user) {
			return json(
				{
					success: false,
					message: 'Invalid username or password'
				},
				{ status: 401 }
			);
		}

		// Type cast to access password field
		const userWithPassword = user as unknown as UserWithPassword;

		if (!userWithPassword.password) {
			return json(
				{
					success: false,
					message: 'Invalid username or password'
				},
				{ status: 401 }
			);
		}

		// Verify password
		const passwordValid = await bcrypt.compare(password, userWithPassword.password);

		if (!passwordValid) {
			return json(
				{
					success: false,
					message: 'Invalid username or password'
				},
				{ status: 401 }
			);
		}

		// Return success with minimal user data
		return json({
			success: true,
			user: {
				id: user.id,
				username: user.name
			}
		});
	} catch (error) {
		console.error('Login error:', error);
		return json(
			{
				success: false,
				message: 'An error occurred during authentication'
			},
			{ status: 500 }
		);
	}
};
